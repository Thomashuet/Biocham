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
 * LoggerThread.java
 * by Sylvain Soliman */

package fr.inria.contraintes.biocham;

import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public class LoggerThread extends Thread {
   BiochamGUI bc;
   JTextArea output;
   BufferedReader input;
   JavaPlot plot;
   ParamTable param, initVal;
 
   public LoggerThread(BiochamGUI biocham) {
      super();
      bc = biocham;
      output = biocham.output;
      input = biocham.bcIn;
      param = biocham.paramTable;
      initVal = biocham.initValTable;
      plot = biocham.plot;
   }

   public void run() {
      boolean eof = false;
      
      while (!eof) {
         try {
            if (input.ready()) {
               String line = input.readLine();
               if (line.startsWith("[GUI] plot "))
                  plot.rePlot(line.substring(11));
               else if (line.startsWith("[GUI] pplot ")) {
                  int i = line.substring(12).indexOf(' ');
                  String s1 = line.substring(12,12+i);
                  int j = line.substring(12+i+1).indexOf(' ');
                  String s2 = line.substring(12+i+1,12+i+1+j);
                  plot.rePlot(line.substring(12+i+2+j));
                  JavaPlot jp = new JavaPlot(s1,s2, plot);
                  bc.tabbedPane.add(jp);
                  bc.tabbedPane.setSelectedIndex(bc.tabbedPane.getTabCount()-1);
               }
               else if (line.startsWith("[GUI] ppplot ")) {
                  int i = line.substring(13).indexOf(' ');
                  String s1 = line.substring(13,13+i);
                  int j = line.substring(13+i+1).indexOf(' ');
                  String s2 = line.substring(13+i+1,13+i+1+j);
                  int k = line.substring(13+i+2+j).indexOf(' ');
                  String s3 = line.substring(13+i+2+j,13+i+2+j+k);
                  plot.rePlot(line.substring(13+i+3+j+k));
                  JavaPlot jp = new JavaPlot(s1,s2, s3, plot);
                  bc.tabbedPane.add(jp);
                  bc.tabbedPane.setSelectedIndex(bc.tabbedPane.getTabCount()-1);
               }
               else if (line.startsWith("[GUI] kplot")) {
                  plot = new JavaPlot(bc.tabbedPane, bc.popupMenu);
                  bc.tabbedPane.add("Plot", plot);
                  bc.tabbedPane.setSelectedIndex(bc.tabbedPane.getTabCount()-1);
               }
               else if (line.startsWith("[GUI] dot ")) {
                  int i = 0;
                  // look for an existing 'Dot' pane
                  while (i < bc.tabbedPane.getTabCount() && !bc.tabbedPane.getTitleAt(i).equals("Dot"))
                     i++;
                  ImageIcon fooimg = new ImageIcon(line.substring(10));
                  // refresh because of the cache
                  if (i < bc.tabbedPane.getTabCount()) {
                     fooimg.getImage().flush();
                     bc.tabbedPane.setSelectedIndex(i);
                     bc.tabbedPane.repaint(i);
                  }
                  // if no 'Dot' exists, create one
                  else {
                     JPanel foo = new JPanel();
                     foo.add(new JLabel(fooimg));
                     bc.tabbedPane.add("Dot", new JScrollPane(foo));
                     bc.tabbedPane.setSelectedIndex(bc.tabbedPane.getTabCount()-1);
                  }
               }
               else if (line.startsWith("[GUI] file ")) {
                  String file = line.substring(11);
                  JTextArea foo = new JTextArea();
                  foo.setFont(new Font("Monospaced",Font.PLAIN,12));
                  foo.setEditable(false);
                  try {
                     BufferedReader in = new BufferedReader(new FileReader(file));
                     String s;
                     while ((s = in.readLine()) != null) {
                        foo.append(s+"\n");
                     }
                  } catch (Exception ex) {
                     System.err.println(ex);
                  }
                  bc.tabbedPane.add(new JScrollPane(foo), file.substring(file.lastIndexOf(File.separator)+1));
                  //bc.tabbedPane.setSelectedIndex(bc.tabbedPane.getTabCount()-1);
               }
               else if (line.startsWith("[GUI] param ")) {
                  int i = line.substring(12).indexOf(',');
                  String p = line.substring(12,12+i);
                  int j = line.length();
                  if (line.charAt(j-1) == '.')
                        j--;
                  float v = Float.parseFloat(line.substring(12+i+1,j));
                  param.setValue(p,v+"");
               }
               else if (line.startsWith("[GUI] initv ")) {
                  int i = line.substring(12).lastIndexOf(',');
                  String p = line.substring(12,12+i);
                  //double v = Double.parseDouble(line.substring(12+i+1));
                  String v = line.substring(12+i+1);
                  if (v.matches("\\d+"))
                     initVal.setValue(p,v+".0");
                  else
                     initVal.setValue(p,v);
               }
               else if (line.startsWith("[GUI] show")) {
                  JDialog showDialog = new JDialog(bc.frame,true);
                  showDialog.setTitle("Shown molecules");
                  showDialog.setLayout(new GridLayout(0,3));
                  ActionListener al = new ActionListener () {
                     public void actionPerformed(ActionEvent e) {
                        JCheckBox jcb = (JCheckBox) e.getSource();
                        if (jcb.isSelected()) {
                           bc.bcOut.append("show");
                           output.append("show");
                        } else {
                           bc.bcOut.append("hide");
                           output.append("hide");
                        }
                        bc.bcOut.append("_molecules("+jcb.getText()+").\n");
                        output.append("_molecules("+jcb.getText()+").\n");
                     }
                  };
                  int nbMol=0;
                  int maxMol=0;
                  while (!(line = input.readLine()).startsWith("[GUI] hide")) {
                     nbMol++;
                     if (line.length()>maxMol)
                        maxMol = line.length();
                     JCheckBox jcb = new JCheckBox(
                        line.substring(5),
                        line.startsWith("show "));
                     jcb.addActionListener(al);
                     showDialog.add(jcb);
                  }
                  Dimension dim = new Dimension(
                     (maxMol-5)*24+60,
                     (((nbMol-1)/3)+3)*20);
                  showDialog.setSize(dim);
                  Point pos = bc.frame.getLocationOnScreen();
                  showDialog.setLocation(
                     pos.x+bc.frame.getSize().width/2-dim.width/2,
                     pos.y+bc.frame.getSize().height/2-dim.height/2);
                  showDialog.setResizable(true);
                  showDialog.setVisible(true);
               }
               else if (line.startsWith("[GUI] quit")) {
                  bc.exit();
               }
               else if (line.startsWith("[GUI] clear_initial_state")) {
                  param.reset();
                  initVal.reset();
               }
               else if (line.startsWith("[GUI] ")) {
               }
               else {
                  if (line.startsWith("** There were "))
                     Toolkit.getDefaultToolkit().beep();
                  if (line.startsWith("biocham: ")) {
                     output.append(line);
                  }
                  else {
                     output.append(line + "\n");
                  }
                  output.setCaretPosition(output.getDocument().getLength());
               }
            } else {
               sleep(500);
            }
         } catch (EOFException e) {
            eof = true;
         } catch (Exception e) {}
      }
   }
}
