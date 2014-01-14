package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultGraphCell;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.menus.ColorMenu;
import fr.inria.contraintes.biocham.menus.CustomPopupMenu;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JColorChooser;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;
import javax.swing.Timer;

public class PopupMenuBiochamCompartment  extends CustomPopupMenu{

	JPopupMenu menu;
	BiochamGraph graph;
	DefaultGraphCell component;
	PopupMenuBiochamCompartment instance;
	BiochamGraphListener listener;
	Timer clickTimer;
	
	public PopupMenuBiochamCompartment(BiochamGraph p,BiochamGraphListener list){
		graph=p;
		instance=this;
		listener=list;
	}
	
	public JPopupMenu getPopup() {
		
		menu = newPopupMenu(); 
		
		
		boolean b=SwingUtilities.isEventDispatchThread();
		
		JMenuItem changeColorMenuItem = newMenuItem("Edit"); 
		changeColorMenuItem.setActionCommand("edit");
		changeColorMenuItem.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
				
				if(component!=null){			
					String newValue= (String)JOptionPane.showInputDialog(BiochamMainFrame.frame,"Compartment name:\n",""); 
					if(newValue!=null && newValue!=""){
						BiochamCompartmentData dt=(BiochamCompartmentData)component.getUserObject();
						dt.setCompartmentName(newValue);
						component.setUserObject(dt);	
					}					
				}				
			}});	  
	    menu.add(changeColorMenuItem);
	 
		
		
		
	    
		JMenuItem editMenuItem = newMenuItem("Change Color"); 
		editMenuItem.setActionCommand("changeColor");
		editMenuItem.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
				if(component!=null){			
					Color newColor=JColorChooser.showDialog(BiochamMainFrame.frame, "Compartment Color", null);
					BiochamCompartmentData d=(BiochamCompartmentData)component.getUserObject();
					d.setColor(newColor);
					component.setUserObject(d);					
					newColor=null;
					d=null;
				}					
			}});	  
	    menu.add(editMenuItem);
	  
	   
	    
	         		  
		return menu;
	}

	public void setComponent(DefaultGraphCell c2) {
		component=c2;
		
	}

	public DefaultGraphCell getComponent() {
		return component;
	}

	public PopupMenuBiochamCompartment getInstance() {
		return instance;
	}

	public void setInstance(PopupMenuBiochamCompartment instance) {
		this.instance = instance;
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
