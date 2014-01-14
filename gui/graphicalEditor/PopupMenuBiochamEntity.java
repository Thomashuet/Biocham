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
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;
import javax.swing.Timer;



public class PopupMenuBiochamEntity extends CustomPopupMenu{

	JPopupMenu menu;
	BiochamGraph graph;
	DefaultGraphCell component;
	PopupMenuBiochamEntity instance;
	BiochamGraphListener listener;
	Timer clickTimer;
	
	
	public PopupMenuBiochamEntity(BiochamGraph p,BiochamGraphListener list){
		graph=p;
		instance=this;
		listener=list;
	}
	
	public JPopupMenu getPopup() {
		
		menu = newPopupMenu(); 
		
		
		boolean b=SwingUtilities.isEventDispatchThread();
		
		JMenuItem changeColorMenuItem = newMenuItem("Change Color"); 
		changeColorMenuItem.setActionCommand("color");
		changeColorMenuItem.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
				
				if(component!=null){			
					Color newColor=JColorChooser.showDialog(BiochamMainFrame.frame, "Molecule Color", null);
					BiochamEntityData d=(BiochamEntityData)component.getUserObject();
					d.setColor(newColor);
					component.setUserObject(d);					
					newColor=null;
					d=null;
				}				
			}});	  
	    menu.add(changeColorMenuItem);
	 
		
		
		String action="Compact";
		if(component!=null){
			BiochamEntityData dt=(BiochamEntityData)component.getUserObject();
			if(dt.isCompact()){
				action="Expand";
			}
		}
		if(component!=null){
			if(component instanceof EComplexCell){
				JMenuItem compactMenuItem = newMenuItem(action);    
			    compactMenuItem.setActionCommand(action);
			    compactMenuItem.addActionListener(new ActionListener(){

					public void actionPerformed(ActionEvent e) {
						if(component!=null){
							BiochamEntityData dt=(BiochamEntityData)component.getUserObject();
							if(e.getActionCommand().equals("Compact")){
								dt.setCompact(true);							
							}else{
								dt.setCompact(false);					
							}
							graph.refresh();
							clickTimer=new Timer(1, new ActionListener(){
									public void actionPerformed(ActionEvent e){
										graph.refresh();
									}
								});
							clickTimer.setRepeats(false); //after expiring once, stop the timer
							clickTimer.start();
						}		
					}});
			    menu.add(compactMenuItem);	  
			   
			}
		}
		
	    
		JMenuItem editMenuItem = newMenuItem("Properties"); 
		editMenuItem.setActionCommand("editBiochamEntity");
		editMenuItem.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
				DefaultGraphCell mol=instance.getComponent();
				if(mol!=null){			
					listener.editBiochamEntity(mol);
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

	public PopupMenuBiochamEntity getInstance() {
		return instance;
	}

	public void setInstance(PopupMenuBiochamEntity instance) {
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
