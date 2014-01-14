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
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Toolkit;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.StringTokenizer;

import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;


/**
 * A class giving the welcome screen of Biocham, an image that appears while the GUI is loading.
 * The purpose of this introduction screen is to notify the user that the program is in process 
 * of loading. The splash screen disappears when the application's main program {@link BiochamGUI}
 * appears.
 *            
 * @author Sylvain Soliman         
*/
public class BiochamSplash extends JFrame {
  
	
   static Image splashImage;
   static boolean imageLoaded = false;
   public JFrame frame;
   /**
    * @param     filename: The image filename for the splash screen.
    */
   BiochamSplash (String fileName) { 
      super();
      setUndecorated(true);
	frame=this;
      setIconImage(Utils.createImage("projectImages/SSicon.png"));
      setVisible(true);
      new AsyncImageLoader(this, fileName);
   }

   
   /**
    * An inner static class that creates in a separate thread a splash screen of the given image filename 
    * as an argument for its constructor.
    */
   static class AsyncImageLoader implements Runnable {   
	  String imageFileName;
      Thread loaderThread;
      BiochamSplash parentFrame;
      
      public AsyncImageLoader(BiochamSplash parent, String fileName){
         parentFrame   = parent;
         imageFileName = fileName;
         loaderThread  = new Thread(this);
         loaderThread.start();
      }

      /**
       * It tracks the status and waits for the image to be loaded.
       * @see java.awt.MediaTracker
      */
      public void run() {
    	  
    	 splashImage = Utils.createImage(imageFileName);              
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
            Utils.debugMsg("Biocham splashloader error loading image \"" +
                    imageFileName + "\"");

            /**
             * This isn't a fatal error - continue.
            */ 
            return;
         }

         /**
          * Resize and frame to match size of image, and keep centered.
         */  
         parentFrame.positionAtCenter(splashImage.getWidth(null), 
               splashImage.getHeight(null));

         /**
          * Signal a redraw, so the image can be displayed.
         */  
         imageLoaded = true;
         parentFrame.repaint();
      }
   }

   
   
   
   /**
    *  Positions the window at the centre of the screen, taking into account
    *  the specified width and height.
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

	    
	  private boolean validateGUI(){
		
		  boolean start=true;
 	  	//JOptionPane.showMessageDialog(frame,"Starting Validating GUI...", "Information",JOptionPane.WARNING_MESSAGE);

		  try{			  
			  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());				
		  }catch(Exception e){
			  Utils.debugMsg("Unable to load native look and feel");
 	  	//JOptionPane.showMessageDialog(frame,"LOOK AND FEEL...", "Information",JOptionPane.WARNING_MESSAGE);

		  }
		
		
		  if(Utils.JAVA_VERSION!=null){
			 
			  StringTokenizer st=new StringTokenizer(Utils.JAVA_VERSION,".");
			  String f=st.nextToken();
			  String s=st.nextToken();
			  if(Float.parseFloat(f+"."+s)<1.5){
				  JOptionPane.showMessageDialog(frame,"You have to install at least the minimum required java version (1.5). ", "Information",JOptionPane.WARNING_MESSAGE);
				  start=false;
				  setVisible(false);
				  dispose();
			  }
		  }else{
			  JOptionPane.showMessageDialog(frame,"You have to install at least the minimum required java version (1.5). ", "Information",JOptionPane.WARNING_MESSAGE);
			  start=false;
			  setVisible(false);
			  dispose();
		  }
		
		  
		  boolean withBiocham=false;
		  String deploymentDir=Utils.getDeploymentDirectory(this.getClass());
		  Utils.debugMsg("lastCHAR="+deploymentDir.substring(deploymentDir.length()-1));
		  Utils.debugMsg("File.separator="+File.separator);
		  if(!deploymentDir.substring(deploymentDir.length()-1).equals(File.separator)){
			  deploymentDir+=File.separator;
		  }
		  Utils.debugMsg("DEPLOYMENT_DIR="+deploymentDir);
		  Utils.DEPLOYMENT_DIR=deploymentDir;
		  String[] files=null;
		  String parentDIR=null;
		  
		  if(Utils.is_OS_MAC || Utils.is_OS_LINUX){
			  
			  String ssMAC=deploymentDir.substring(0,deploymentDir.length()-1);
			  ssMAC=ssMAC.substring(0,ssMAC.lastIndexOf("/"));
			  parentDIR=File.separator+ssMAC;
			  Utils.debugMsg("PARENT DIR="+parentDIR);
			  File fMAC=new File(parentDIR);
			  Utils.debugMsg("PARENT DIR exists="+fMAC.exists());			 
			  files=fMAC.list();
			  Utils.debugMsg("1-files.length="+files.length);
			  Utils.DEPLOYMENT_DIR=File.separator+deploymentDir;			
			  Utils.resourcesDIR=parentDIR+File.separator+"gui"+File.separator;
			 
		  }else{
	   	  	//JOptionPane.showMessageDialog(frame,"deploymentDir="+deploymentDir,"Information",JOptionPane.WARNING_MESSAGE);
			  File f1=new File((new File(deploymentDir)).getParent());
			  Utils.debugMsg("path="+f1.toString());
			  Utils.debugMsg("path="+f1.getAbsolutePath());
			  files=f1.list();	
			 // String ssMAC=deploymentDir.substring(0,deploymentDir.length()-1);
			 // ssMAC=ssMAC.substring(0,ssMAC.lastIndexOf("/"));
			 // parentDIR=ssMAC.substring(0,ssMAC.lastIndexOf("/")+1);
			//  Utils.debugMsg("*****PARENT DIR="+parentDIR);
		  
			  parentDIR=f1.getPath();
			  Utils.debugMsg("Parent dir="+parentDIR+", files.size="+files.length);
			//JOptionPane.showMessageDialog(frame,"parentDir="+parentDIR,"Information",JOptionPane.WARNING_MESSAGE);

			  
		  }
		  
  		 //JOptionPane.showMessageDialog(frame,"Searching for biocham Started...","Information",JOptionPane.WARNING_MESSAGE);
		  if(files!=null){
	       		Arrays.sort(files);
	       		for(int i=0;i<files.length;i++){
	       			Utils.debugMsg(files[i]);
	       			if(files[i].equals("biocham") || files[i].equals("biocham.exe")){
	       				withBiocham=true;
	       				//biochamDir=parentDIR;
	       				break;
	       			}
	       		}
		  }		
		//JOptionPane.showMessageDialog(frame,"Searching for biocham Finished...WithBiocham="+withBiocham,"Information",JOptionPane.WARNING_MESSAGE);
		/* withBiocham=true;
		 parentDIR="/nas/home3/d/djovanov/biocham3.3";*/
		 
		 if(withBiocham){
	    	  //JOptionPane.showMessageDialog(frame,"Biocham INTEGRATED found: "+Utils.BIOCHAM_DIR, "Information",JOptionPane.WARNING_MESSAGE);

	    	  Utils.BIOCHAM_DIR=parentDIR;
	    	  Utils.debugMsg("With integrated biocham FOUND!!!!Utils.BIOCHAM_DIR="+Utils.BIOCHAM_DIR);
	    	  Utils.BIOCHAM_VERSION=parentDIR;
	    	  
	      }else{	    	
	    	  JOptionPane.showMessageDialog(frame,"Biocham couldn't be found or the minimum required biocham version 3.0 is not found. ", "Information",JOptionPane.WARNING_MESSAGE);
	    	  start=false;
	    	  setVisible(false);
			  dispose();
	      }
	     		 	 
		  if(withBiocham){
			  Utils.WORKING_DIR=Utils.BIOCHAM_DIR;
			//JOptionPane.showMessageDialog(frame,"Utils.WORKING_DIR="+Utils.WORKING_DIR,"Information",JOptionPane.WARNING_MESSAGE);

		  }else{
			//JOptionPane.showMessageDialog(frame,"without biocham,1 Utils.WORKING_DIR="+Utils.WORKING_DIR,"Information",JOptionPane.WARNING_MESSAGE);
			  Utils.WORKING_DIR=System.getProperty("user.dir");
			//JOptionPane.showMessageDialog(frame,"without biocham,2 Utils.WORKING_DIR="+Utils.WORKING_DIR,"Information",JOptionPane.WARNING_MESSAGE);

		  }
		 //JOptionPane.showMessageDialog(frame,"return from validate is: "+start, "Information",JOptionPane.WARNING_MESSAGE);

		  return start;
		  
	  }
     
   public static final void main(String [] args){
	   
	      
	   if(args!=null){
		   if(args.length>0){
			   if(args[0]!="" && args[0]!=" "){
				   if(args[0].equals("--debug")){
					   Utils.mode_debug=true;
					   Utils.debugMsg("DEBUG MODE IS ON.\n");
				   }
			   }
		   }
	   }
	  
	   try {
		Utils.getResourceListing(BiochamSplash.class,"");
	} catch (URISyntaxException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} catch (IOException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	}
	   
	   final BiochamSplash f = new BiochamSplash("projectImages/splash.png");
	   if(f.validateGUI()){

//	JOptionPane.showMessageDialog(f,"Validate GUI OK", "Information",JOptionPane.WARNING_MESSAGE);

		   new Icons();	
		   Icons.loadAllImages();		  
		   SwingUtilities.invokeLater(new Runnable(){

				public void run() {
					  new BiochamMainFrame(f);
					
				}});
	   }else{
	//JOptionPane.showMessageDialog(f,"Validate GUI NO", "Information",JOptionPane.WARNING_MESSAGE);

		}
	   	   
//	   /new BiochamMainFrame(f);
	   /*SwingUtilities.invokeLater(new Runnable(){

		public void run() {
	//JOptionPane.showMessageDialog(f,"Starting BiochamMainFrame...", "Information",JOptionPane.WARNING_MESSAGE);
			  new BiochamMainFrame(f);
			
		}});
	   */
   }
   
   
   
}
