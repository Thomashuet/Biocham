/* BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
 * Copyright 2003-2008, INRIA, Projet Contraintes
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * JavaPlot.java
 * by Sylvain Soliman */

package fr.inria.contraintes.biocham;

import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import java.text.*;
import java.util.prefs.*;
import java.util.concurrent.locks.*;

public class JavaPlot extends JPanel implements MouseListener {
   MyCanvas drawingArea;
   MyCaption caption;
   LinkedList<Float>[] data;    // data and time can be accessed by repaint()!
   Vector<Float> time;  // and can be modified by plot
   ReentrantLock lock;  // we need to synchronize
   Color[] color;
   String[] name;
   String[] fullname;
   LinkedList<Float>[] fdata;
   Float[][] fulldata;
   int[] column;
   Random r;
   int hsize;
   boolean ready;
   float xmin,xmax,ymin,ymax,zmin,zmax;
   float mintime,maxdata,mindata;
   String filename;
   JColorChooser cc;
   JLabel coord;
   int xoff = 40;
   int yoff = 20;
   JPanel captionPane;
   int currentColor;
   int type;         // time series, 2 var phase plot, 3 var...
   int xindex, yindex, zindex;
   String xname, yname, zname;
   int parentIndex;
   static int globalIndex = 0;
   JTabbedPane jtp;
   JPopupMenu popupMenu;
   static final int DRAWPOINTS = 1;
   static final int TOOLTIPS = 2;
   static final int DEFAULTPREFS = 2;
   int drawPrefs;
   JCheckBox drawBox;
   JCheckBox ttipsBox;
      
   
   // Principal simulation
   JavaPlot(JTabbedPane tabbedPane, JPopupMenu popup) {
      ready = false;
      data = null;
      color = null;
      name = null;
      lock = new ReentrantLock();

      setLayout(new BorderLayout());
      
      drawingArea = new MyCanvas();
      add(drawingArea,BorderLayout.CENTER);
      
      drawPrefs = Preferences.userNodeForPackage(this.getClass()).
         getInt("drawPrefs",DEFAULTPREFS);

      captionPane = new JPanel();
      captionPane.setLayout(new BorderLayout());
      captionPane.setBackground(Color.white);
      
      caption = new MyCaption();
      captionPane.add(caption,BorderLayout.CENTER);
      
      JPanel jp = new JPanel(new GridLayout(0,1));
      drawBox = new JCheckBox("Draw Points",
            (drawPrefs & DRAWPOINTS) == DRAWPOINTS);
      drawBox.addActionListener(caption);
      drawBox.setBackground(Color.white);
      jp.add(drawBox);
      if (! drawBox.isOpaque())
         drawBox.setOpaque(true);
         
      ttipsBox = new JCheckBox("Show Tooltips",
            (drawPrefs & TOOLTIPS) == TOOLTIPS);
      ttipsBox.addActionListener(caption);
      ttipsBox.setBackground(Color.white);
      jp.add(ttipsBox);
      if (! ttipsBox.isOpaque())
         ttipsBox.setOpaque(true);

      captionPane.add(jp,BorderLayout.PAGE_START);

      coord = new JLabel("0.0, 0.0", JLabel.TRAILING);
      coord.setPreferredSize(new Dimension(90,10));
      captionPane.add(coord,BorderLayout.PAGE_END);
      
      add(captionPane,BorderLayout.LINE_END);

      setMinimumSize(new Dimension(100, 30));
      setPreferredSize(new Dimension(650, 500));
      
      r = new Random();
      column = new int[256];
      cc = new JColorChooser();
   
      type = 1;
      zname = null;
      parentIndex = globalIndex;
      globalIndex++;
      jtp = tabbedPane;
      popupMenu = popup;
   }	

   // Phase space curve
   JavaPlot(String s1, String s2, JavaPlot jplot) {
      color = null;
      lock = new ReentrantLock();
      setLayout(new BorderLayout());

      drawingArea = new MyCanvas();
      add(drawingArea,BorderLayout.CENTER);

      captionPane = null;
      caption = null;

      coord = new JLabel("0.0, 0.0", JLabel.TRAILING);
      setBackground(Color.white);

      coord.setPreferredSize(new Dimension(90,10));

      add(coord,BorderLayout.PAGE_END);

      setMinimumSize(new Dimension(100, 30));
      setPreferredSize(new Dimension(650, 500));

      r = null;
      column = null;
      cc = null;

      setName(s1+" vs. "+s2);
      xname = s1;
      yname = s2;
      zname = null;
      
      update(jplot);
      type = 2;

      parentIndex = jplot.parentIndex;
      popupMenu = jplot.popupMenu;
   }

   // 3D Phase space curve
   JavaPlot(String s1, String s2, String s3, JavaPlot jplot) {
      color = null;
      lock = new ReentrantLock();
      setLayout(new BorderLayout());

      drawingArea = new MyCanvas();
      add(drawingArea,BorderLayout.CENTER);

      captionPane = null;
      caption = null;

      coord = new JLabel("0.0, 0.0", JLabel.TRAILING);
      setBackground(Color.white);

      coord.setPreferredSize(new Dimension(90,10));

      //add(coord,BorderLayout.PAGE_END);

      setMinimumSize(new Dimension(100, 30));
      setPreferredSize(new Dimension(650, 500));

      r = null;
      column = null;
      cc = null;

      setName(s1+" vs. "+s2+" vs. "+s3);

      xname = s1;
      yname = s2;
      zname = s3;
      
      update(jplot);

      type = 3;

      parentIndex = jplot.parentIndex;
      popupMenu = jplot.popupMenu;
   }

   void update(JavaPlot jplot) {
      ready = jplot.ready;
      data = jplot.data;
      name = jplot.name;
      
      xindex = -1;
      yindex = -1;
      zindex = -1;
      
      if (ready) {
         for (int i=0;i<name.length;i++)
         {
            if (xname.equals(name[i]))
               xindex = i;
            if (yname.equals(name[i]))
               yindex = i;
            if (zname != null && zname.equals(name[i]))
               zindex = i;
         }
         
         if (xindex>=0 && yindex >=0 && (zname == null || zindex > 0)) {
            xmax = Float.NEGATIVE_INFINITY;
            xmin = Float.POSITIVE_INFINITY;
            ymax = Float.NEGATIVE_INFINITY;
            ymin = Float.POSITIVE_INFINITY;

            LinkedList<Float> il = data[xindex];
            LinkedList<Float> jl = data[yindex];
            
            LinkedList<Float> kl = null;
            if (zindex > 0) {
               zmax = Float.NEGATIVE_INFINITY;
               zmin = Float.POSITIVE_INFINITY;
               kl = data[zindex];
            }

            for (int index = 0; index < il.size() ; ++index) {
               xmax = Math.max(xmax, il.get(index));
               xmin = Math.min(xmin, il.get(index));
               ymax = Math.max(ymax, jl.get(index));
               ymin = Math.min(ymin, jl.get(index));
               if (zindex > 0) {
                  zmax = Math.max(zmax, kl.get(index));
                  zmin = Math.min(zmin, kl.get(index));
               }
            }

            if (zindex > 0) {
               float x,y;
               x = to2Dx(xmin,ymin,zmin);
               y = to2Dy(xmax,ymin,zmin);
               xmin = x;
               ymin = y;
               x = to2Dx(xmax,ymax,zmax);
               y = to2Dy(xmin,ymax,zmax);
               xmax = x;
               ymax = y;
            }

            if (isEnabled())
               repaint();
         } else
            ((JTabbedPane) getParent()).remove(this);
      }  
   }

   void rePlot(String file) {
      //System.err.println("plotting: "+file);
      filename = file;
      try {
         lock.lock();
         //System.err.println("got lock");
         try {
            readData(readPlot());
         } finally {
            lock.unlock();
            //System.err.println("released lock");
         }
         //System.err.println("data read, repainting... ");
         caption.setHSize(hsize*8);
         validate();
         drawingArea.repaint();
         caption.repaint();
      } catch (Exception e) {
         System.err.println(filename + " not found!\n" + e);
      }
      // update secondary tabs
      for (int i=jtp.getTabCount()-1;i>0;--i)
      {
         Object cmp = jtp.getComponentAt(i);
         if(cmp.getClass() == this.getClass()) {
            JavaPlot jp = ((JavaPlot) cmp);
            if (jp != this && jp.parentIndex == parentIndex)
               jp.update(this);
         }
      }
   }
   
   public void readData(String fileName) throws IOException {
      BufferedReader in = new BufferedReader(new FileReader(fileName));
      String s = null;
      String[] tmp_name = new String[256];
      mintime = Float.POSITIVE_INFINITY;
      maxdata = Float.NEGATIVE_INFINITY;
      mindata = Float.POSITIVE_INFINITY;
      tmp_name = in.readLine().split("\\t");
      fullname = new String[tmp_name.length];
      fdata = (LinkedList<Float> []) new LinkedList[tmp_name.length];
      for (int i=0;i<tmp_name.length;i++) {
         fdata[i] = new LinkedList<Float> ();
         fullname[i]=tmp_name[i];
      }
      fullname[0] = "Time";
      while ((s = in.readLine()) != null) {
         readDataFromLine(s);
      }
      fulldata = new Float[fdata.length][fdata[0].size()];
      for (int i=0;i<fdata.length;++i) {
         fdata[i].toArray(fulldata[i]);
      }
      ready = true;
   }

   public void setXmax (String s) {
      if (s.charAt(0) == '*')
         xmax=-1;
      else xmax = Float.parseFloat(s);
   }

   public void setXmin (String s) {
      if (s.charAt(0) == '*')
         xmin=-1;
      else xmin = Float.parseFloat(s);
   }

   public void setYmax (String s) {
      if (s.charAt(0) == '*')
         ymax=-1;
      else ymax = Float.parseFloat(s);
   }

   public void setYmin (String s) {
      if (s.charAt(0) == '*')
         ymin=-1;
      else ymin = Float.parseFloat(s);
   }


   public String readPlot() throws IOException {
      BufferedReader in = new BufferedReader(new FileReader(filename));
      int i;
      int rr,gg,bb;
      String dataFile,s;
      String[] line, plot;
      String[] tmp_name = new String[256];
      
      for (i=0;i<256;++i)
         column[i] = -1;   // default: do not show
      
      dataFile = "";
      hsize = 8;  // min size of right pane in chars
      while ((s = in.readLine()) != null) {
         if (s.startsWith("plot")) {
            if (dataFile == "") {
               line = s.split(" |\"");
               dataFile = line[2];
            }
            plot = s.split(", ");
            for (i=0;i<plot.length;i++) {
               line = plot[i].split(" |\"");
               if (i == 0) {
                  // get name
                  tmp_name[i] = line[8];
                  // get column
                  column[Integer.parseInt(line[5].substring(2))] = i;
               }
               else {
                  tmp_name[i] = line[7];
                  column[Integer.parseInt(line[4].substring(2))] = i;
               }
               if (tmp_name[i].length() > hsize)
                  hsize = tmp_name[i].length();
            }
         }
         else if (s.startsWith("set xrange [")) {
            int limit = s.indexOf(':');
            setXmin(s.substring(12,limit));
            setXmax(s.substring(limit+1,s.indexOf(']')));
         }
         else if (s.startsWith("set yrange [")) {
            int limit = s.indexOf(':');
            setYmin(s.substring(12,limit));
            setYmax(s.substring(limit+1,s.indexOf(']')));
         }
      }
      drawingArea.s.clear(); // clear zoom stack
      // Moved to after releasing the lock
      //validate();
      //repaint();
      data = (LinkedList<Float> []) new LinkedList[i];
      for (int j=0; j<i; ++j)
         data[j] = new LinkedList<Float> ();
      color = new Color[i];
      name = new String[i];
      time = new Vector<Float>();

      for (int j=0;j<i;++j) {
         name[j]=tmp_name[j];
         String colstr = Preferences.userNodeForPackage(this.getClass()).get(name[j],"");
         if (colstr.equals("")) {
            rr = r.nextInt(256);
            gg = r.nextInt(256);
            bb = r.nextInt(256);
            NumberFormat nf = NumberFormat.getInstance();
            nf.setMinimumIntegerDigits(3);
            Preferences.userNodeForPackage(this.getClass()).
               put(name[j],nf.format(rr)+nf.format(gg)+nf.format(bb));
         } else  {
            rr = Integer.parseInt(colstr.substring(0,3));
            gg = Integer.parseInt(colstr.substring(3,6));
            bb = Integer.parseInt(colstr.substring(6,9));
         }
         color[j] = new Color(rr,gg,bb);
      }

      return dataFile;
   }

   void readDataFromLine(String s) {
      int i,j;
      
      if (s.charAt(0) == '#') // comment
         return;

      String[] line = s.split("\\t");
      
      // add first column data to time
      time.add(Float.parseFloat(line[0]));
      fdata[0].addFirst(Float.parseFloat(line[0]));
      if (time.lastElement() < mintime)
         mintime = time.lastElement();
      
      // add the rest where it belongs
      for (i=1;i<line.length;i++) {
         fdata[i].addFirst(Float.parseFloat(line[i]));
         if ((j = column[i+1]) >= 0) {
            data[j].addFirst(Float.parseFloat(line[i]));
            if (data[j].element() > maxdata)
               maxdata = data[j].element();
            if (data[j].element() < mindata)
               mindata = data[j].element();
         }
      }
   }

   void plot(Graphics g, int x, int y) {
      Color c;
      double x0,y0,dx,dy,tx,ty;
      LinkedList<Float> il,jl,kl;
      
      // erase
      g.setColor(Color.white);
      g.fillRect(0,0,x,y);

      if (!ready || data == null || data.length < 1 || x < 0)
         return;

      // border caption
      x -= xoff+10;
      y -= 2*yoff;
      
      if (xmin == -1)
         xmin = mintime;
      if (xmax == -1)
         xmax = time.lastElement();
      if (ymin == -1)
         ymin = mindata;
      if (ymax == -1)
         ymax = maxdata;
      dx = ((double) x)/(xmax - xmin);
      dy = ((double) y)/(ymax - ymin);
      
      // tics
      tx = tic(xmax - xmin);
      ty = tic(ymax - ymin);

      g.setColor(Color.black);
      if (type != 3 && xmin <= 0 && xmax >= 0) {
         g.drawLine((int) (-xmin*dx) + xoff ,yoff ,(int) (-xmin*dx) + xoff,y+yoff);
         for (y0 = Math.ceil(ymin/ty)*ty;y0<ymax;y0+=ty) {
            g.drawLine((int) (-xmin*dx) + xoff, y+yoff-(int)((y0 - ymin)*dy) ,
                  (int) (-xmin*dx) + xoff + 2 , y+yoff-(int)((y0 - ymin)*dy));
            g.drawString(relevant(y0+""),2,y+yoff-(int)((y0 - ymin)*dy)+5);
         }
      }
      if (type !=3 && ymin <= 0 && ymax >= 0) {
         g.drawLine(xoff,y+yoff+(int) (ymin*dy),x+xoff,y+yoff+(int) (ymin*dy));
         for (x0 = Math.ceil(xmin/tx)*tx;x0<xmax;x0+=tx) {
            g.drawLine((int) ((x0 - xmin)*dx)+xoff, y+yoff+(int)(ymin*dy) ,
                  (int) ((x0 - xmin)*dx)+xoff, y+yoff+(int)(ymin*dy)-2);
            g.drawString(relevant(x0+""),(int) ((x0 - xmin)*dx)+xoff - 11, y+yoff+12);
         }
      }

      
      // drawing per se
      switch (type) {
         
         case 1:
            for (int i=0;i<data.length;++i) {
               g.setColor(color[i]);

               il = data[i];
               y0 = il.element();
               x0 = time.lastElement();
               int index = 1;;
               int xx0 = (int) ((x0 - xmin)*dx);
               int yy0 = (int) ((y0 - ymin)*dy);

               for (int j=time.size()-2;j>=0;j--) {
                  float x1 = time.elementAt(j);
                  float y1 = il.get(index);

                  int xx1 = (int) ((x1 - xmin)*dx);
                  int yy1 = (int) ((y1 - ymin)*dy);
                  // line
                  if (xx1 < xx0) {
                     g.drawLine(xx0+xoff,
                           y + yoff - yy0,
                           xx1+xoff,
                           y + yoff - yy1);
                     // point
                     if ((drawPrefs & DRAWPOINTS) == DRAWPOINTS) {
                        g.drawLine(xx0+xoff,
                           y + yoff - yy0 - 2,
                           xx0+xoff,
                           y + yoff - yy0 + 2);
                     }
                     x0 = x1;
                     y0 = y1;
                     xx0 = xx1;
                     yy0 = yy1;
                  }
                  index++;
               }
            }
            break;

         case 2:
            g.setColor(Color.red);
            il = data[xindex];
            jl = data[yindex];
            x0 = il.element();
            y0 = jl.element();
            for (int index=1; index < il.size();++index) {
               float x1 = il.get(index);
               float y1 = jl.get(index);
               g.drawLine((int) ((x0 - xmin)*dx)+xoff,y + yoff - (int) ((y0 - ymin)*dy),
                     (int) ((x1 - xmin)*dx)+xoff,y + yoff - (int) ((y1 - ymin)*dy));
               x0 = x1;
               y0 = y1;
            }
            break;

         case 3:
            g.setColor(Color.black);
            x0 = to2Dx(0,0,0);
            y0 = to2Dy(0,0,0);
            
            float x1 = to2Dx(0,0,1);
            float y1 = to2Dy(0,0,1);
            
            g.drawLine((int) ((x0 - xmin)*dx)+xoff,y + yoff - (int) ((y0 - ymin)*dy),
                  (int) ((x1 - xmin)*dx)+xoff,y + yoff - (int) ((y1 - ymin)*dy));
            
            x1 = to2Dx(1,0,0);
            y1 = to2Dy(1,0,0);
            
            g.drawLine((int) ((x0 - xmin)*dx)+xoff,y + yoff - (int) ((y0 - ymin)*dy),
                  (int) ((x1 - xmin)*dx)+xoff,y + yoff - (int) ((y1 - ymin)*dy));
            
            x1 = to2Dx(0,1,0);
            y1 = to2Dy(0,1,0);
            
            g.drawLine((int) ((x0 - xmin)*dx)+xoff,y + yoff - (int) ((y0 - ymin)*dy),
                  (int) ((x1 - xmin)*dx)+xoff,y + yoff - (int) ((y1 - ymin)*dy));
            
            g.setColor(Color.red);
            il = data[xindex];
            jl = data[yindex];
            kl = data[zindex];

            x0 = to2Dx(il.element(),jl.element(),kl.element());
            y0 = to2Dy(il.element(),jl.element(),kl.element());
            for (int index=1; index < il.size(); ++index) {
               x1 = to2Dx(il.get(index),jl.get(index),kl.get(index));
               y1 = to2Dy(il.get(index),jl.get(index),kl.get(index));
               g.drawLine((int) ((x0 - xmin)*dx)+xoff,y + yoff - (int) ((y0 - ymin)*dy),
                     (int) ((x1 - xmin)*dx)+xoff,y + yoff - (int) ((y1 - ymin)*dy));
               x0 = x1;
               y0 = y1;
            }
            break;
      }
   }

   float to2Dy(float x, float y, float z) {
      double theta1 = - Math.PI/18;
      double theta2 = Math.PI/9;
      
      return (float) (x*Math.sin(theta1)+y*Math.sin(theta2)+z);
   }
   
   float to2Dx(float x, float y, float z) {
      double theta1 = - Math.PI/18;
      double theta2 = Math.PI/9;
      
      return (float) (x*Math.cos(theta1)+y*Math.cos(theta2));
   }
   
   double tic(float range) {
      double u = Math.pow(10,Math.floor(Math.log(range)/Math.log(10)));
      double c = Math.ceil(range/u);
      double m = 1;
      
      if (c > 1 && c <= 2)
         m = 2;
      if (c > 2 && c <= 5)
         m = 5;
      if (c > 5)
         m = 10;
      
      return m*u/10;
   }

   void showSelection(Graphics g, int x0, int y0, int width, int height) {
      g.setColor(Color.black);
      g.drawRect(x0,y0,width,height);
   }

   double getRealX(int x) {
      // For x xoff on one side and 10 only on the other...
      double xx = (double) drawingArea.getSize().width - xoff - 10;
      double dx = xx/(xmax - xmin);

      xx = ((double)(x - xoff))/dx + xmin;
      if (xx < 0)
         xx = 0;
      
      return xx;
   }
   
   double getRealY(int y) {
      double yy = (double) drawingArea.getSize().height - 2*yoff;
      double dy = yy/(ymax - ymin);
      
      yy = (yy + yoff - y)/dy + ymin;
      if (yy < 0)
         yy = 0;

      return yy;
   }
   
   void setCoord(int x, int y) {
      double xx = getRealX(x);
      double yy = getRealY(y);
      
      coord.setText(relevant(xx+"") + ", " + relevant(yy+""));
   }

   String relevant(String nb) {
      if (nb.length()<5)
         return nb;
      
      int i = nb.indexOf("E");
      if (i>0 && i<4)
         return nb;
      if (i>0)
         return nb.substring(0,4)+nb.substring(i);
      
      i = nb.indexOf(".");
      if (i<0)
         return nb;
      if (i>2)
         return nb.substring(0,i);
      return nb.substring(0,5);
   }

   // MouseListener methods
   public void mouseClicked(MouseEvent e) {
      if (jtp != null) {
         int index = jtp.indexAtLocation(e.getX(), e.getY());
         if (index > -1)
         {
            Component cmp = jtp.getComponentAt(index);
            if (e.getClickCount() == 2) {
               JavaPlot jp = null;
               if (cmp.getClass() == this.getClass())
                  jp = (JavaPlot) jtp.getComponentAt(index);
               if (cmp.getClass() != this.getClass() ||
                     ((jp.type > 1) || (jp.parentIndex < globalIndex-1))) {
                  int prev = jtp.getSelectedIndex();
                  if (prev == index)
                     jtp.setSelectedIndex(Math.max(index-1,0));
                  jtp.removeTabAt(index);
               }
            }
         }
      }
   }

   public void mousePressed(MouseEvent e) {}

   public void mouseReleased(MouseEvent e) {}

   public void mouseEntered(MouseEvent e) {}

   public void mouseExited(MouseEvent e) {}

   /**Drawing pane*/
   class MyCanvas extends JPanel implements MouseMotionListener, MouseListener {
      Cursor c;
      int x1, y1, x2, y2;  // selection rectangle
      Stack<Float> s;      // stack of successive zooms

      MyCanvas() {
         c = new Cursor(Cursor.CROSSHAIR_CURSOR);
         setCursor(c);
         addMouseMotionListener(this);
         addMouseListener(this);
         x1 = -1;
         x2 = -1;
         y1 = -1;
         y2 = -1;
         s = new Stack<Float>();
      }

      public void paintComponent(Graphics gr) {
         int x = getSize(null).width;
         int y = getSize(null).height;

         if (x>0) {
            //System.err.println("repainting MyCanvas... ");
            lock.lock();
            try {
               //System.err.println("got lock");
               plot(gr,x,y);
               
               int x0 = Math.min(x1,x2);
               int y0 = Math.min(y1,y2);
               if (x0 >= 0 && y0 >= 0) {
                  showSelection(gr, x0, y0,
                        Math.max(x1,x2)-x0, Math.max(y1,y2)-y0);
               }
            } finally {
               lock.unlock();
               //System.err.println("released lock");
            }
            //System.err.println("MyCanvas done.");
         }
      }

      // MouseMotionListener methods
      public void mouseMoved(MouseEvent e) {
         setCoord(e.getX(),e.getY());
         if (time != null && ((drawPrefs & TOOLTIPS) == TOOLTIPS)) {
            double x = getRealX(e.getX());
            double y = getRealY(e.getY());
            int i=0;
            while ((i<time.size()) && (time.elementAt(i) < x))
               i++;
            if ((i >= time.size()) ||
                  ((i>0) && (x-time.elementAt(i-1) < time.elementAt(i)-x)))
               i--;
            x = y;
            int k=0;
            i = fulldata[0].length-i-1;
            for (int j=0;j<fulldata.length;++j)
               // is shown and is closer
               if (column[j+1]>=0 && Math.abs(fulldata[j][i] - y) < x) {
                  x = Math.abs(fulldata[j][i] - y);
                  k = j;
               }
            if (k>0)
               setToolTipText(fullname[k]);
            else
               setToolTipText(null);
         }
      }

      public void mouseDragged(MouseEvent e) {
         x2 = Math.min(e.getX(),getSize(null).width-1);
         y2 = Math.min(e.getY(),getSize(null).height-1);
         x2 = Math.max(0,x2);
         y2 = Math.max(0,y2);
         repaint();
      }

      // MouseListener methods
      // go back to previous zoom on double-click
      public void mouseClicked(MouseEvent e) {
         if (e.getClickCount() == 2 && !(s.empty())) {
            ymax = s.pop();
            xmax = s.pop();
            ymin = s.pop();
            xmin = s.pop();
            x1 = -1;
            y1 = -1;
            x2 = -1;
            y2 = -1;
            repaint();
         }
      }

      public void mousePressed(MouseEvent e) {
         if (e.isPopupTrigger() && type == 1) {
            // contextual menu only for main plots
            popupMenu.show(e.getComponent(), e.getX(), e.getY());
         } else {
            x1 = e.getX();
            y1 = e.getY();
            x2 = -1;
            y2 = -1;
         }
      }

      public void mouseReleased(MouseEvent e) {
         if (x1 >= 0 && x2 >=0 && y1 >=0 && y2 >= 0
               && Math.abs(x2-x1)>2 && Math.abs(y2-y1)>2) {
            float new_xmin = (float) getRealX(Math.min(x1,x2));
            float new_ymax = (float) getRealY(Math.min(y1,y2));
            float new_xmax = (float) getRealX(Math.max(x1,x2));
            float new_ymin = (float) getRealY(Math.max(y1,y2));
            
            s.push(xmin);
            s.push(ymin);
            s.push(xmax);
            s.push(ymax);
            
            xmin = new_xmin;
            ymin = new_ymin;
            xmax = new_xmax;
            ymax = new_ymax;
         }
         x1 = -1;
         y1 = -1;
         x2 = -1;
         y2 = -1;
         repaint();
      }

      public void mouseEntered(MouseEvent e) {}

      public void mouseExited(MouseEvent e) {}
   }

   /**Caption*/
   class MyCaption extends JPanel implements MouseListener, ActionListener {

      MyCaption() {
         addMouseListener(this);
      }

      public void setHSize (int hsize) {
         setSize(new Dimension(hsize+20,getSize().height));
         setPreferredSize(new Dimension(hsize+20,getSize().height));
      }

      public void paintComponent(Graphics g) {
         g.setColor(Color.white);
         g.fillRect(0,0,getSize().width,getSize().height);
         //System.err.println("repainting MyCaption... ");
         lock.lock();
         try {
            //System.err.println("got lock");
            if (data != null)
               for (int i=0;i<data.length;++i) {
                  g.setColor(color[i]);
                  g.drawLine(0,(i+1)*12,10,(i+1)*12);
                  g.setColor(Color.black);
                  if (name[i] != null)
                     g.drawString(name[i],20,(i+1)*12+6);
               }
         } finally {
            lock.unlock();
            //System.err.println("released lock");
         }
         //System.err.println("MyCaption done.");
      }

      // MouseListener methods
      public void mouseClicked(MouseEvent e) {
         if (color == null)
            return;

         int i;
         if ((i = e.getY()-6) >= 0 && (i = i/12) < color.length) {
            currentColor = i;
            setColor(i, cc.showDialog(this,"Choose a color",color[i]));
         }
      }
      public void mousePressed(MouseEvent e) {}
      public void mouseReleased(MouseEvent e) {}
      public void mouseEntered(MouseEvent e) {}
      public void mouseExited(MouseEvent e) {}

      /**Action dispatcher.
       *
       * sends to BIOCHAM actions triggered by menus.
       */
      public void actionPerformed(ActionEvent e) {
         Object source = e.getSource();

         if (source == drawBox) {
            if (drawBox.isSelected())
               drawPrefs = drawPrefs | DRAWPOINTS;
            else
               drawPrefs = drawPrefs & ~DRAWPOINTS;
            Preferences.userNodeForPackage(this.getClass()).
               putInt("drawPrefs",drawPrefs);
            drawingArea.repaint();
         }

         if (source == ttipsBox) {
            if (ttipsBox.isSelected())
               drawPrefs = drawPrefs | TOOLTIPS;
            else {
               drawPrefs = drawPrefs & ~TOOLTIPS;
               drawingArea.setToolTipText(null);
            }
            Preferences.userNodeForPackage(this.getClass()).
               putInt("drawPrefs",drawPrefs);
         }
      }

      public void setColor(int i, Color c) {
         if (c != null & c != color[i]) {
            color[i] = c;
            NumberFormat nf = NumberFormat.getInstance();
            nf.setMinimumIntegerDigits(3);
            Preferences.userNodeForPackage(this.getClass()).
               put(name[i],nf.format(c.getRed())
                     +nf.format(c.getGreen())
                     +nf.format(c.getBlue()));
            repaint();
            drawingArea.repaint();
         }
      }
   }
}
