package fr.inria.contraintes.biocham.menus;

import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.customComponents.BiochamPlot;
import fr.inria.contraintes.biocham.customComponents.BiochamPlot.PlotCanvas;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;

public class CustomPopupBiochamPlot extends CustomPopupMenu{

	JPopupMenu menu;
	PlotCanvas drawingArea;
	
	public CustomPopupBiochamPlot(PlotCanvas p){
		drawingArea=p;
	}
	
	public JPopupMenu getPopup() {
		
		menu = newPopupMenu(); 
		
		
		boolean b=SwingUtilities.isEventDispatchThread();
		
		
		JMenuItem openMenuItem = newMenuItem("Save As"); 
		openMenuItem.setActionCommand("savePlotImage");
	    openMenuItem.addActionListener(drawingArea);
	    menu.add(openMenuItem);
	   	   
	    menu.addSeparator();
	    
	    JMenuItem createMenuItem = newMenuItem("Fit to Screen");    
	    createMenuItem.setActionCommand("fitPlot");
	    createMenuItem.addActionListener(drawingArea);
	    menu.add(createMenuItem);
	    
	    JMenuItem clearMenuItem = newMenuItem("Customize Plot");    
    	clearMenuItem.setActionCommand("customizePlot");
    	clearMenuItem.addActionListener(drawingArea);
    	menu.add(clearMenuItem);
    	
    	menu.addSeparator();
    	
    	JMenuItem compareMenuItem = newMenuItem("Add to Comparison Window"); 
 		compareMenuItem.setActionCommand("addToComparison");
 		compareMenuItem.addActionListener(drawingArea);
 	    menu.add(compareMenuItem);
    		  
    	menu.addSeparator();
    	
    	JMenuItem viewDataMenuItem = newMenuItem("View data"); 
   	    viewDataMenuItem.setActionCommand("viewPlotData");
   	    viewDataMenuItem.addActionListener(drawingArea);
   	    menu.add(viewDataMenuItem);
   	    
   	    JMenuItem setInitMenuItem = newMenuItem("Set Initial State from trace"); 
   	    setInitMenuItem.setActionCommand("setInitFromTrace");
   	    setInitMenuItem.addActionListener(drawingArea);
	    menu.add(setInitMenuItem);
     	 
  	    JMenuItem plotExperimentDataMenuItem = newMenuItem("Plot Experimental Data"); 
	    plotExperimentDataMenuItem.setActionCommand("plotExperimentalData");
	    plotExperimentDataMenuItem.addActionListener(drawingArea);
	    menu.add(plotExperimentDataMenuItem);
	   
  	   
	   
	   /* JMenuItem printMenuItem = newMenuItem("Print"); 
	    printMenuItem.setActionCommand("printPlotImage");
	    printMenuItem.addActionListener(drawingArea);
	    menu.add(printMenuItem);*/
	    
		return menu;
	}

	@Override
	public ColorMenu getMenu() {
		
		return null;
	}

	@Override
	public JPopupMenu getPMenu() {
		return menu;
	}

	@Override
	public void setMenu(ColorMenu menu) {
				
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
