package fr.inria.contraintes.biocham.utils;


import java.awt.Image;
import java.awt.image.BufferedImage;
import java.util.HashMap;

import javax.swing.ImageIcon;
import javax.swing.SwingUtilities;

public class Icons {

	
	   
	//public static ArrayList<String> welcomeImagesNames;
	//public static ArrayList<Image> welcomeImages;
	public static HashMap<String,Image> images;
	public static HashMap<String,BufferedImage> bufferedImages;
	public static HashMap<String,ImageIcon> icons;
	public static final String IMAGES_DIR ="projectImages/";
	
	
	public Icons(){
		images=new HashMap<String,Image>();
		icons=new HashMap<String,ImageIcon>();
		bufferedImages=new HashMap<String,BufferedImage>();
	}
	
	
	public static void addImage(String name){
		
		boolean b=SwingUtilities.isEventDispatchThread();
		//System.out.println("\n *****image: "+IMAGES_DIR+name);
		images.put(name, Utils.createImage(IMAGES_DIR+name));
	}
	
	public static void addBufferedImage(String name){
		
		boolean b=SwingUtilities.isEventDispatchThread();
		//System.out.println("\n *****image: "+IMAGES_DIR+name);
		bufferedImages.put(name, Utils.createBufferedImage(IMAGES_DIR+name));
	}
	
	
	public static void addImageIcon(String name){
		
		boolean b=SwingUtilities.isEventDispatchThread();
		//System.out.println("\n *****image: "+IMAGES_DIR+name);

		icons.put(name, Utils.createImageIcon(IMAGES_DIR+name));
	}
	
	public static void addImageIcon(String name,Double d){
		
		//System.out.println("\n *****image: "+IMAGES_DIR+name);
		boolean b=SwingUtilities.isEventDispatchThread();
		
		icons.put(name+d, Utils.createImageIcon(IMAGES_DIR+name,d));
	}


	public static void loadAllImages() {
	
		

		
      Icons.addImageIcon("addCompartment.png",0.15);
      //Icons.addImageIcon("applications.png");
      //Icons.addImageIcon("applications.png",0.7);
      Icons.addImageIcon("modify1.png");
      
      Icons.addImageIcon("taskic.jpg",0.5);
     // Icons.addImageIcon("yellow.png",0.05);
      //Icons.addImageIcon("green_puzzle.jpg",0.05);
     // Icons.addImageIcon("puzzle_purple.jpg",0.05);
      
      //Icons.addImageIcon("blueCurve.png",0.1);
     // Icons.addImageIcon("kreversi.png");
      
      //Icons.addImageIcon("saveas16.png",1.35);
     // Icons.addImageIcon("SSicon_Add2.png",0.5);
      //Icons.addImageIcon("fileclose-32.png",0.7);
      
      //Icons.addImageIcon("modify2.png");
      Icons.addImageIcon("delete3.gif");
      //Icons.addImageIcon("delete4.gif");
      Icons.addImageIcon("article-32.png");
      
      Icons.addImageIcon("chart-icon2.png");
            
      Icons.addImageIcon("gEditorIcon.png",0.4);
	  Icons.addImageIcon("addCompartment.png",0.20);
      Icons.addImageIcon("undo1.png",0.4);
	  Icons.addImageIcon("infoAssist.png",0.7);	
   	  Icons.addImageIcon("info.png",0.35); 
   	  Icons.addImageIcon("folderblue.png");
   	  Icons.addImageIcon("new_file.png", 0.4);
   	  Icons.addImageIcon("new_file03.png", 0.4);
   	  Icons.addImageIcon("export_file.png", 0.4);
   	  Icons.addImageIcon("45.png", 1.5);
   	  Icons.addImageIcon("46.png", 1.5);
   	  Icons.addImageIcon("85.png");
   	  Icons.addImageIcon("21.png");
   	  Icons.addImageIcon("81.png");
   	  Icons.addImageIcon("print.png");
   	  Icons.addImageIcon("fileclose-32.png", 0.8);
   	  Icons.addImageIcon("Refresh3.png");
   	  Icons.addImageIcon("order.png", 0.5);
   	  Icons.addImageIcon("graph.png", 0.3);
   	  Icons.addImageIcon("biochamStop3.png", 0.7);
   	  Icons.addImageIcon("chart_organisation.png", 1.8);
   	  Icons.addImageIcon("abstractionIcon.png", 0.8);
   	  Icons.addImageIcon("chart_curve.png", 1.8);
      Icons.addImageIcon("chart_curve.png",5.0);
   	  Icons.addImageIcon("article-32.png");
   	  Icons.addImageIcon("explorer-32.png");
   	  Icons.addImageIcon("paste.png");
   	  Icons.addImageIcon("remove3.png");   
   	  Icons.addImageIcon("computer_48.png",0.5);
   	  Icons.addImageIcon("agt_internet-32.png",0.8);
   	  Icons.addImageIcon("Info_32.png");
   	  Icons.addImageIcon("dark-blue-circle.png");
   	  Icons.addImageIcon("SSicon.png");
   	  Icons.addImageIcon("SSicon.png",0.4);
   	  //Icons.addImageIcon("rotated_splash.png",2.0);
   	 // Icons.addImageIcon("psr_hq.jpg");
   	  Icons.addImageIcon("mol1.jpeg");
   	  Icons.addImageIcon("page1-water-permeation.jpg");
   	  Icons.addImageIcon("longmult_normal.jpg");
   	 
   	  Icons.addImageIcon("SSicon.png",0.8);
   	  Icons.addImageIcon("Books-icon.gif");  	  
   	  Icons.addImageIcon("application_xp_terminal.png",1.4);
   	  Icons.addImageIcon("page_white_error.png",1.5);
   	  Icons.addImageIcon("Ok-32x32.png");
   	  Icons.addImageIcon("Ok-32x32.png",0.5);
   	  Icons.addImageIcon("File-1-48x48.png",1.5);
   	  Icons.addImageIcon("flag_blue.png");
   	  Icons.addImageIcon("metalcomplex.gif",0.1);
   	  Icons.addImageIcon("customize.png",1.8);
   	  Icons.addImageIcon("redAsterisk.gif",0.01);
   	  Icons.addImageIcon("molecule1.png",0.2);     	  
   	  Icons.addImageIcon("p10.png",0.1);   	  
   	  Icons.addImageIcon("cut2.png",0.6);
   	  Icons.addImageIcon("copy.png");
   	  Icons.addImageIcon("pastee.png");
   	  Icons.addImageIcon("undoW.png");
  	  Icons.addImageIcon("redoW.png");
  	  Icons.addImageIcon("ZoomIn.png");
 	  Icons.addImageIcon("ZoomOut.png");
 	  Icons.addImageIcon("remove.png"); 	  
	  Icons.addImageIcon("cut2.png",0.8);
   	  Icons.addImageIcon("copy.png",1.2);
   	  Icons.addImageIcon("pastee.png",1.2);
   	  Icons.addImageIcon("undoW.png",1.2);
  	  Icons.addImageIcon("redoW.png",1.2);
  	  Icons.addImageIcon("ZoomIn.png",1.2);
 	  Icons.addImageIcon("ZoomOut.png",1.2);
 	  Icons.addImageIcon("remove.png",1.2); 	  
 	  Icons.addImageIcon("addReactant.png",0.6);
 	  Icons.addImageIcon("StateTransition1.png",0.6);
 	  Icons.addImageIcon("addProduct.png",0.6);
 	  Icons.addImageIcon("addModulator.png",0.6);
 	  Icons.addImageIcon("Association1.png",0.6);
 	  Icons.addImageIcon("Dissociation1.png",0.6); 	 
 	  Icons.addImageIcon("addReactant.png",0.65);
 	  Icons.addImageIcon("StateTransition1.png",0.65);
 	  Icons.addImageIcon("addProduct.png",0.65);
 	  Icons.addImageIcon("addModulator.png",0.65);
 	  Icons.addImageIcon("Association1.png",0.65);
 	  Icons.addImageIcon("Dissociation1.png",0.65);
	  Icons.addImageIcon("molecule2.png",0.1);
	  Icons.addImageIcon("kompare_111_32.png");	 	
 	 Icons.addImageIcon("atom1.png",0.5);
 	 Icons.addImageIcon("warning_16.png");
 	 Icons.addImageIcon("question.png"); 	
 	  Icons.addImage("greenIcon.png");
 	  Icons.addImage("rotated_splash.png");
 	 Icons.addImage("molecule2.png");
 	 Icons.addImage("water_molecule-icon5.png");
	 Icons.addImage("kreversi.png");
	 Icons.addImage("chart_curve.png");	 
		
	}	
	
}
