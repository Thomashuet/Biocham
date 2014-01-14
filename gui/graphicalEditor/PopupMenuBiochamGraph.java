package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultGraphCell;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.menus.ColorMenu;
import fr.inria.contraintes.biocham.menus.CustomPopupMenu;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JColorChooser;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.Timer;

public class PopupMenuBiochamGraph extends CustomPopupMenu{

	

	JPopupMenu menu;
	BiochamGraph graph;
	Timer clickTimer;
	
	public PopupMenuBiochamGraph(BiochamGraph g){
		graph=g;
	}

	@Override
	public JPopupMenu getPopup() {
		
		menu = newPopupMenu(); 
		/*JMenuItem syncMenuItem = newMenuItem("Syncronize");
	    menu.add(syncMenuItem);*/
	   
	    String action="Compact/Expand";
	    JMenuItem createMenuItem = newMenuItem(action);
	    createMenuItem.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
				
				//GraphUtilities.hasReversibleReaction(graph);
				boolean compact=!graph.isCompactGraph();
				Object[] objs=GraphUtilities.getAllMolecules(graph);
				for(int i=0;i<objs.length;i++){
					if(objs[i] instanceof EComplexCell){
						GraphUtilities.getBiochamEntityDataFromCell(objs[i]).setCompact(compact);
					}
				}
				graph.setCompactGraph(compact);
				graph.refresh();
				clickTimer=new Timer(1, new ActionListener(){
						public void actionPerformed(ActionEvent e){
							graph.refresh();
						}
					});
				clickTimer.setRepeats(false); //after expiring once, stop the timer
				clickTimer.start();
				
				
			}
	    	
	    });
	    menu.add(createMenuItem);
	    
	    JMenuItem exportImgMenuItem = newMenuItem("Export image"); 
	    exportImgMenuItem.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
				String rep=Utils.showSaveDialog("",BiochamMainFrame.frame,"image");			
				if (rep!=null) {				
					graph.exportAsImage(rep);
				}
				
			}});
	    menu.add(exportImgMenuItem);
	    
	    JMenuItem refreshMenuItem = newMenuItem("Refresh"); 
	    refreshMenuItem.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
				graph.refresh();
			/*	BiochamGraph.graphEditorFrame.revalidate();
				BiochamGraphEditorDesktop.graphDesktop.revalidate();
				BiochamMainFrame.frame.validate();
				BiochamMainFrame.frame.repaint();*/
				/*BiochamGraph.graphEditorFrame.repaint();
				BiochamGraphEditorDesktop.graphDesktop.repaint();*/
				
			}});
	    menu.add(refreshMenuItem);	    
	    
	         		  
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
