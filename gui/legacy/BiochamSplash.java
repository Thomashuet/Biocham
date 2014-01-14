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
 * BiochamSplash.java
 * by Sylvain Soliman */

package fr.inria.contraintes.biocham;

import java.awt.*;
import javax.swing.*;

public class BiochamSplash extends JFrame {
   static Image   splashImage;
   static boolean imageLoaded = false;

   BiochamSplash (String fileName) {
      super();
      setUndecorated(true);
      setIconImage(Toolkit.getDefaultToolkit().createImage(BiochamSplash.class.getResource("images/SSicon.png")));
      setVisible(true);
      new AsyncImageLoader(this, fileName);
   }

   static class AsyncImageLoader implements Runnable {
      String  imageFileName;
      Thread  loaderThread;
      BiochamSplash  parentFrame;

      public AsyncImageLoader(BiochamSplash parent, String fileName){
         parentFrame   = parent;
         imageFileName = fileName;
         loaderThread  = new Thread(this);
         loaderThread.start();
      }

      public void run() {
         splashImage = Toolkit.getDefaultToolkit().createImage(BiochamSplash.class.getResource(imageFileName));

         // wait for the image to be loaded
         MediaTracker tracker = new MediaTracker(parentFrame);
         tracker.addImage(splashImage,0);
         try {
            tracker.waitForID(0);
         }
         catch(InterruptedException e){
            e.printStackTrace();
            System.exit(1);
         }

         if(tracker.isErrorID(0)){
            System.err.println("Biocham splashloader error loading image \"" +
                  imageFileName + "\"");

            // this isn't a fatal error - continue
            return;
         }

         // resize and frame to match size of image, and keep centered
         parentFrame.positionAtCenter(splashImage.getWidth(null), 
               splashImage.getHeight(null));

         // signal a redraw, so the image can be displayed
         imageLoaded = true;
         parentFrame.repaint();
      }
   }

   /**
    *  Positions the window at the centre of the screen, taking into account
    *  the specified width and height
    */
   private void positionAtCenter(int width, int height){
      Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
      setBounds((screenSize.width-width)/2,
            (screenSize.height-height)/2,
            width,
            height);
   }

   public void paint(Graphics g){
      if(imageLoaded){
         g.drawImage(splashImage,0,0,null);
      }
   }

   public void update(Graphics g){
      paint(g);
   }

   public static final void main(String [] args){
      BiochamSplash f = new BiochamSplash("images/splash.png");

      try {
         // call BiochamGUI main method
         Class.forName("fr.inria.contraintes.biocham.BiochamGUI").getMethod("main", new Class[] {String[].class}).invoke(null, new Object[] {args});

         f.setVisible(false);
         f.dispose();
      }
      catch(Exception e){
         System.err.println("Biocham splashloader error: "+e);
         System.exit(-1);
      }
   }
}
