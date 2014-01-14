package fr.inria.contraintes.biocham.menus;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.BiochamModel;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;


public class CustomPopupPlotImage extends CustomPopupMenu{

	
	JPopupMenu menu;
	BiochamDynamicTree tree;
	
	public CustomPopupPlotImage(BiochamDynamicTree tree){
		this.tree=tree;
	}
	
	
	public JPopupMenu getPopup() {
		
		menu = newPopupMenu(); 
	    
		boolean b=SwingUtilities.isEventDispatchThread();
		
		
		JMenuItem compareMenuItem = newMenuItem("Comparison Window"); 
		compareMenuItem.setActionCommand("comparePlots");
		compareMenuItem.addActionListener(this.tree.treeListener);
	    menu.add(compareMenuItem);
	    
	    JMenuItem plotExperimentDataMenuItem = newMenuItem("Plot Experimental Data"); 
	    plotExperimentDataMenuItem.setActionCommand("plotExperimentalData");
	    plotExperimentDataMenuItem.addActionListener(this.tree.treeListener);
	    menu.add(plotExperimentDataMenuItem);
	    
	  /*  JMenuItem printMenuItem = newMenuItem("Print"); 
	    printMenuItem.setActionCommand("printPlotImage");
	    printMenuItem.addActionListener(this.tree);
	    menu.add(printMenuItem);
	    */
	    JMenuItem viewDataMenuItem = newMenuItem("View data"); 
	    viewDataMenuItem.setActionCommand("viewPlotData");
	    viewDataMenuItem.addActionListener(this.tree.treeListener);
	    menu.add(viewDataMenuItem);
	    
	    
	    menu.addSeparator();
	    
	    JMenuItem saveAsMenuItem = newMenuItem("Save As"); 
	    saveAsMenuItem.setActionCommand("savePlotImage");
	    saveAsMenuItem.addActionListener(this.tree.treeListener);
	    menu.add(saveAsMenuItem);
	    
	   
	    	   
		
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
