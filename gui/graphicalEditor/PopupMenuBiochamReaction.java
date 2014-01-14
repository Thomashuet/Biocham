package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultGraphCell;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.menus.ColorMenu;
import fr.inria.contraintes.biocham.menus.CustomPopupMenu;
import fr.inria.contraintes.biocham.modelData.ParamTableRules;

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

public class PopupMenuBiochamReaction extends CustomPopupMenu{

	JPopupMenu menu;
	BiochamGraph graph;
	DefaultGraphCell component;
	PopupMenuBiochamReaction instance;
	BiochamGraphListener listener;
	Timer clickTimer;
	
	public PopupMenuBiochamReaction(BiochamGraph p,BiochamGraphListener list){
		graph=p;
		instance=this;
		listener=list;
	}
	
	public JPopupMenu getPopup() {
		
		menu = newPopupMenu(); 
		
		
		boolean b=SwingUtilities.isEventDispatchThread();
		
		 if(!(component instanceof Dissociation)){
			    JMenuItem addReactantMenuItem = newMenuItem("Add Reactant"); 
			    addReactantMenuItem.setActionCommand("addReactant");
			    addReactantMenuItem.addActionListener(new ActionListener(){		
					public void actionPerformed(ActionEvent e) {
						if(component instanceof StateTransition){
							StateTransition st=(StateTransition)component;
							boolean allowed=true;
							for(int i=0;i<st.getSources().size();i++){					
								if(st.getSources().get(i).getParentName().equals("Source/Sink")){
									allowed=false;
								}
							}
							if(allowed){
								st.addReactant();
							}else{
								JOptionPane.showMessageDialog(graph.getBiochamModel().getGraphEditor(),"You can't add a reactant when there is Source/Sink as a reactant already.","Warning",JOptionPane.WARNING_MESSAGE);
							}
							st=null;
						}else if(component instanceof Association){
							Association st=(Association)component;
							st.addReactant();
						}else if(component instanceof ReversibleAssociation){
							ReversibleAssociation st=(ReversibleAssociation)component;
							st.addReactant();
						}else if(component instanceof ReversibleStateTransition){
							ReversibleStateTransition st=(ReversibleStateTransition)component;							
							boolean allowed=true;
							for(int i=0;i<st.getSources().size();i++){					
								if(st.getSources().get(i).getParentName().equals("Source/Sink")){
									allowed=false;
								}
							}
							if(allowed){
								st.addReactant();
							}else{
								JOptionPane.showMessageDialog(graph.getBiochamModel().getGraphEditor(),"You can't add a reactant when there is Source/Sink as a reactant already.","Warning",JOptionPane.WARNING_MESSAGE);
							}
							st=null;							
						}
					}				
					});	  
			    menu.add(addReactantMenuItem);			  
		    }
		    if(!(component instanceof Association)){
			    JMenuItem addProductMenuItem = newMenuItem("Add Product"); 
			    addProductMenuItem.setActionCommand("addProduct");
			    addProductMenuItem.addActionListener(new ActionListener(){		
					public void actionPerformed(ActionEvent e) {
						if(component instanceof StateTransition){
							StateTransition st=(StateTransition)component;
							boolean allowed=true;
							for(int i=0;i<st.getTargets().size();i++){
								if(st.getTargets().get(i).getParentName().equals("Source/Sink")){
									allowed=false;
								}
							}
							if(allowed){
								st.addProduct();
							}else{
								JOptionPane.showMessageDialog(graph.getBiochamModel().getGraphEditor(),"You can't add a product when there is Source/Sink as a product already.","Warning",JOptionPane.WARNING_MESSAGE);
							}
							st=null;				
						}else if(component instanceof Dissociation){
							Dissociation st=(Dissociation)component;
							st.addProduct();
						}else if(component instanceof Association){
							((Association)component).addProduct();
						}else if(component instanceof ReversibleAssociation){
							ReversibleAssociation st=(ReversibleAssociation)component;
							st.addProduct();
						}else if(component instanceof ReversibleStateTransition){
							ReversibleStateTransition st=(ReversibleStateTransition)component;
							boolean allowed=true;
							for(int i=0;i<st.getTargets().size();i++){
								if(st.getTargets().get(i).getParentName().equals("Source/Sink")){
									allowed=false;
								}
							}
							if(allowed){
								st.addProduct();
							}else{
								JOptionPane.showMessageDialog(graph.getBiochamModel().getGraphEditor(),"You can't add a product when there is Source/Sink as a product already.","Warning",JOptionPane.WARNING_MESSAGE);
							}
							st=null;							
						}
					}});	  
			    menu.add(addProductMenuItem);			   
		    }
		    JMenuItem addModulatorMenuItem = newMenuItem("Add Modulator"); 
		    addModulatorMenuItem.setActionCommand("addModulator");
		    addModulatorMenuItem.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e) {//forward or backward		
					if(component instanceof StateTransition){
						StateTransition st=(StateTransition)component;
						st.addModulator();													
					}else if(component instanceof Association){
						Association st=(Association)component;
						st.addModulator();
					}else if(component instanceof Dissociation){
						Dissociation st=(Dissociation)component;
						st.addModulator();
					}else if(component instanceof ReversibleAssociation){
						ReversibleAssociation st=(ReversibleAssociation)component;
						st.addModulator();
					}else if(component instanceof ReversibleStateTransition){
						ReversibleStateTransition st=(ReversibleStateTransition)component;
						st.addModulator();
					}
				}});	  
		    menu.add(addModulatorMenuItem);
		    menu.addSeparator();
		
		    JMenuItem editMenuItem = newMenuItem("Edit"); 
		    editMenuItem.setActionCommand("editData");
		    editMenuItem.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e) {//forward or backward	
					listener.editReaction(component);
				}});	  
		    menu.add(editMenuItem);
		    menu.addSeparator();		   
		    JMenuItem deleteMenuItem = newMenuItem("Delete Reaction"); 
		    deleteMenuItem.setActionCommand("delete");
		    deleteMenuItem.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e) {//forward or backward	
					String reactionName = null;
					if(component instanceof StateTransition){
						StateTransition st=(StateTransition)component;
						reactionName=st.getId();											
					}else if(component instanceof Association){
						Association st=(Association)component;
						reactionName=st.getId();
					}else if(component instanceof Dissociation){
						Dissociation st=(Dissociation)component;
						reactionName=st.getId();
					}else if(component instanceof ReversibleAssociation){
						ReversibleAssociation st=(ReversibleAssociation)component;
						reactionName=st.getId();
					}else if(component instanceof ReversibleStateTransition){
						ReversibleStateTransition st=(ReversibleStateTransition)component;
						reactionName=st.getId();
					}
					if(reactionName!=null){
						((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).deleteRule(reactionName,true,true,true);
						//graph.deleteReaction(reactionName,true);
					}
					
				}});	  
		    menu.add(deleteMenuItem);
		    menu.addSeparator(); 
		  
		JMenuItem verticalMenuItemDown = newMenuItem("Set Vertical Down"); 
		verticalMenuItemDown.setActionCommand("setVerticalDown");
		verticalMenuItemDown.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				if(component instanceof StateTransition){
					StateTransition st=(StateTransition)component;
					((MiddleEdge)st.getMiddleEdge()).setVerticalBT();											
				}else if(component instanceof Association){
					Association st=(Association)component;
					((MiddleEdgeAssociation)st.getMiddleEdge()).setVerticalDown();
				}else if(component instanceof Dissociation){
					Dissociation st=(Dissociation)component;
					((MiddleEdgeDissociation)st.getMiddleEdge()).setVerticalDown();
				}else if(component instanceof ReversibleAssociation){
					ReversibleAssociation st=(ReversibleAssociation)component;
					st.setVerticalDown();
				}else if(component instanceof ReversibleStateTransition){
					ReversibleStateTransition st=(ReversibleStateTransition)component;
					st.setVerticalDown();
				}
			}});	  
	    menu.add(verticalMenuItemDown);
	    JMenuItem verticalMenuItemUp = newMenuItem("Set Vertical Up"); 
	    verticalMenuItemUp.setActionCommand("setVerticalUp");
	    verticalMenuItemUp.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				if(component instanceof StateTransition){
					StateTransition st=(StateTransition)component;
					((MiddleEdge)st.getMiddleEdge()).setVerticalTB();											
				}else if(component instanceof Association){
					Association st=(Association)component;
					((MiddleEdgeAssociation)st.getMiddleEdge()).setVerticalUp();
				}else if(component instanceof Dissociation){
					Dissociation st=(Dissociation)component;
					((MiddleEdgeDissociation)st.getMiddleEdge()).setVerticalUp();
				}else if(component instanceof ReversibleAssociation){
					ReversibleAssociation st=(ReversibleAssociation)component;
					st.setVerticalUp();
				}else if(component instanceof ReversibleStateTransition){
					ReversibleStateTransition st=(ReversibleStateTransition)component;
					st.setVerticalUp();
				}
			}});	  
	    menu.add(verticalMenuItemUp);
	    
	    JMenuItem horizontalMenuItem = newMenuItem("Set Horizontal"); 
	    horizontalMenuItem.setActionCommand("setHorizontal");
	    horizontalMenuItem.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				if(component instanceof StateTransition){
					StateTransition st=(StateTransition)component;
					((MiddleEdge)st.getMiddleEdge()).setHorizontal();											
				}else if(component instanceof Association){
					Association st=(Association)component;
					((MiddleEdgeAssociation)st.getMiddleEdge()).setHorizontal();
				}else if(component instanceof Dissociation){
					Dissociation st=(Dissociation)component;
					((MiddleEdgeDissociation)st.getMiddleEdge()).setHorizontal();
				}else if(component instanceof ReversibleAssociation){
					ReversibleAssociation st=(ReversibleAssociation)component;
					st.setHorizontal();
				}else if(component instanceof ReversibleStateTransition){
					ReversibleStateTransition st=(ReversibleStateTransition)component;
					st.setHorizontal();
				}
			}});	  
	    menu.add(horizontalMenuItem);
	    
	    JMenuItem horizontalMenuItemReverse = newMenuItem("Set Horizontal Reverse"); 
	    horizontalMenuItemReverse.setActionCommand("setHorizontalReverse");
	    horizontalMenuItemReverse.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				if(component instanceof StateTransition){
					StateTransition st=(StateTransition)component;
					((MiddleEdge)st.getMiddleEdge()).setHorizontalReverse();											
				}else if(component instanceof ReversibleStateTransition){
					ReversibleStateTransition st=(ReversibleStateTransition)component;
					st.setHorizontalReverse();
				}else if(component instanceof Association){
					Association st=(Association)component;
					((MiddleEdgeAssociation)st.getMiddleEdge()).setHorizontalReverse();
				}else if(component instanceof Dissociation){
					Dissociation st=(Dissociation)component;
					((MiddleEdgeDissociation)st.getMiddleEdge()).setHorizontalReverse();
				}else if(component instanceof ReversibleAssociation){
					ReversibleAssociation st=(ReversibleAssociation)component;
					st.setHorizontalReverse();
				}
			}});
	    
		menu.add(horizontalMenuItemReverse);	    
	    menu.addSeparator();
	   /* JMenuItem orientationMenuItem = newMenuItem("Change LHS/RHS"); 
	    orientationMenuItem.setActionCommand("lhs_rhs");
	    orientationMenuItem.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				if(component instanceof StateTransition){
					StateTransition st=(StateTransition)component;
					st.changeLhs_Rhs();											
				}else if(component instanceof Association){
					Association st=(Association)component;
					//st.changeLhs_Rhs();
				}else if(component instanceof Dissociation){
					Dissociation st=(Dissociation)component;
					//st.changeLhs_Rhs();
				}else if(component instanceof ReversibleAssociation){
					ReversibleAssociation st=(ReversibleAssociation)component;
					//st.changeLhs_Rhs();
				}else if(component instanceof ReversibleStateTransition){
					ReversibleStateTransition st=(ReversibleStateTransition)component;
					//st.changeLhs_Rhs();
				}
			}});	  
	    menu.add(orientationMenuItem);
	    menu.addSeparator();*/
	    JMenuItem colorMenuItem = newMenuItem("Change Color"); 
	    colorMenuItem.setActionCommand("color");
	    colorMenuItem.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {//forward or backward			
				if(component instanceof StateTransition){
					StateTransition st=(StateTransition)component;
					Color newColor=JColorChooser.showDialog(BiochamMainFrame.frame, "Molecule Color", null);	
					if(newColor==null){
						newColor=Color.black;
					}
					st.setColor(newColor);			
					newColor=null;	
				}else if(component instanceof Association){
					Association st=(Association)component;					
					Color newColor=JColorChooser.showDialog(BiochamMainFrame.frame, "Molecule Color", null);	
					if(newColor==null){
						newColor=Color.black;
					}
					st.setColor(newColor);			
					newColor=null;	
				}else if(component instanceof Dissociation){
					Dissociation st=(Dissociation)component;					
					Color newColor=JColorChooser.showDialog(BiochamMainFrame.frame, "Molecule Color", null);	
					if(newColor==null){
						newColor=Color.black;
					}
					st.setColor(newColor);			
					newColor=null;	
				}else if(component instanceof ReversibleAssociation){
					ReversibleAssociation st=(ReversibleAssociation)component;
					Color newColor=JColorChooser.showDialog(BiochamMainFrame.frame, "Molecule Color", null);	
					if(newColor==null){
						newColor=Color.black;
					}
					st.setColor(newColor);			
					newColor=null;	
				}else if(component instanceof ReversibleStateTransition){
					ReversibleStateTransition st=(ReversibleStateTransition)component;
					Color newColor=JColorChooser.showDialog(BiochamMainFrame.frame, "Molecule Color", null);	
					if(newColor==null){
						newColor=Color.black;
					}
					st.setColor(newColor);			
					newColor=null;	
				}
			}});	  
	    menu.add(colorMenuItem);
	    
	   
	   
		return menu;
	}

	public void setComponent(DefaultGraphCell c2) {
		component=c2;
		
	}

	public DefaultGraphCell getComponent() {
		return component;
	}

	public PopupMenuBiochamReaction getInstance() {
		return instance;
	}

	public void setInstance(PopupMenuBiochamReaction instance) {
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
