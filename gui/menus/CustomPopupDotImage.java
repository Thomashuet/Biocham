package fr.inria.contraintes.biocham.menus;

import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.customComponents.ImageContainer;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.SwingUtilities;

public class CustomPopupDotImage extends CustomPopupMenu{

	JPopupMenu menu;
	ImageContainer parent;
	
	public CustomPopupDotImage(ImageContainer p){
		this.parent=p;		
	}
	
	
	public JPopupMenu getPopup() {
		
	
		menu = newPopupMenu();    
		/*	
		boolean b=SwingUtilities.isEventDispatchThread();
		
	    if(parent.getName()!=null){
	    	if(parent.getName().equals("ReactionGraph")){
	    		JMenuItem editMenuItem = newMenuItem("Edit"); 
	    		editMenuItem.setActionCommand("editRGraph");
	    		editMenuItem.addActionListener(parent);
	    		menu.add(editMenuItem);	    		 
	    		menu.add(new JSeparator());
	    	}
	    }*/
		
	    JMenuItem saveAsMenuItem = newMenuItem("Save As"); 
	    saveAsMenuItem.setActionCommand("saveDotImage");
	    saveAsMenuItem.addActionListener(parent);
	    menu.add(saveAsMenuItem);
	    
	    menu.add(new JSeparator());
	    
	   /* JMenuItem printMenuItem = newMenuItem("Print"); 
	    printMenuItem.setActionCommand("printDotImage");
	    printMenuItem.addActionListener(parent);
	    menu.add(printMenuItem);
	    
	    
	    menu.add(new JSeparator());*/
	    
	    
	    JMenuItem zoomInMenuItem = newMenuItem("Zoom In"); 
	    zoomInMenuItem.setActionCommand("zoomIn");
	    zoomInMenuItem.addActionListener(parent);
	    menu.add(zoomInMenuItem);
	    
	    JMenuItem zoomOutMenuItem = newMenuItem("Zoom Out"); 
	    zoomOutMenuItem.setActionCommand("zoomOut");
	    zoomOutMenuItem.addActionListener(parent);
	    menu.add(zoomOutMenuItem);
	       
	    	   
		
		return menu;
	}


	@Override
	public ColorMenu getMenu() {
		// TODO Auto-generated method stub
		return null;
	}


	@Override
	public JPopupMenu getPMenu() {
		// TODO Auto-generated method stub
		return menu;
	}


	@Override
	public void setMenu(ColorMenu menu) {
		// TODO Auto-generated method stub
		
	}


	@Override
	public void setPMenu(JPopupMenu menu) {
		this.menu=menu;
		
	}


	@Override
	public ColorMenu refreshed(BiochamModel m) {
		// TODO Auto-generated method stub
		return null;
	}


	@Override
	public JPopupMenu refreshedPopup(BiochamModel m) {
		// TODO Auto-generated method stub
		return null;
	}

}
