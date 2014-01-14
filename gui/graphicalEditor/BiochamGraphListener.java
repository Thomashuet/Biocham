package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultGraphCell;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.modelData.ParamTableRules;
import fr.inria.contraintes.biocham.utils.Icons;

import java.awt.Color;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import javax.swing.JColorChooser;
import javax.swing.JOptionPane;




public class BiochamGraphListener implements MouseListener{

	
	
	BiochamGraph graph;
	
	public BiochamGraphListener(BiochamGraph g){
		graph=g;
	}
	


	public void mouseClicked(MouseEvent e) {
		
		
		if(e.getClickCount()==2){
			
			if(graph.getSelectionCell() instanceof DefaultGraphCell){
				
				DefaultGraphCell c=(DefaultGraphCell)graph.getSelectionCell();					
				if(c instanceof StateTransition || c instanceof Association || c instanceof Dissociation || c instanceof ReversibleStateTransition || c instanceof ReversibleAssociation){
					
					//BiochamEdgeData edgeData;
					//String s;
					editReaction(c);	
					
					//edgeData=null;s=null;
				}else if(c instanceof EMacromoleculeCell || c instanceof ENucleicAcidFeatureCell || c instanceof EComplexCell){

					Color newColor=JColorChooser.showDialog(BiochamMainFrame.frame, "Molecule Color", null);
					BiochamEntityData d=(BiochamEntityData)c.getUserObject();
					d.setColor(newColor);
					c.setUserObject(d);					
					newColor=null;
					d=null;
				}				
				c=null;
			}
		}else if(e.getClickCount()==1){
			
			
			
			/*IF ITS A RIGHT-CLICK,LAUNCH THE MOLECULE-EDITOR OR THE REACTION-POPUP DIALOG*/
			
			if ((e.getModifiers() & e.BUTTON3_MASK) == MouseEvent.BUTTON3_MASK) {				         
			
				
				 if(graph.getSelectionCell() instanceof DefaultGraphCell){				
					 
					 DefaultGraphCell c=(DefaultGraphCell)graph.getSelectionCell();				
				
					 if(GraphUtilities.isBiochamEntity(c)){
						 
						 graph.moleculePopup.setComponent(c);					 
						 graph.moleculePopup.getPopup().show(e.getComponent(), e.getX(), e.getY());
					 }else if(GraphUtilities.isBiochamCompartment(c)){
						 graph.compartmentPopup.setComponent(c);
						 graph.compartmentPopup.getPopup().show(e.getComponent(), e.getX(), e.getY());
					 }
				
					/* 
					 					
					 					 * 1.
					 					 * 
					 					 * 					 
					 
					  RIGHT-CLICK ON MOLECULE SHOWS HER DIALOG-EDITOR
					 if(c instanceof EMacromoleculeCell || c instanceof ENucleicAcidFeatureCell){// || c instanceof EComplexCell){
						 editBiochamEntity(c);
						 //graph.moleculePopup.setComponent(c);
						 //graph.moleculePopup.getPopup().show(e.getComponent(), e.getX(), e.getY());									
					}else if(c instanceof EComplexCell){
						graph.moleculePopup.setComponent(c);
						graph.moleculePopup.getPopup().show(e.getComponent(), e.getX(), e.getY());
					}
					 
					 */
					 

									 					/*
									 					 * 
									 					 * 2.
									 					 *  
									 					 * */		
					 					 
					/*RIGHT-CLICK ON REACTION SHOWS ITS POPUP FOR MODIFICATION(VERTICAL,HORIZONTAL,COLOR)*/ 
					
					else if(GraphUtilities.isBiochamReaction(c)){
						graph.reactionPopup.setComponent(c);
						graph.reactionPopup.getPopup().show(e.getComponent(),e.getX(),e.getY());
						//graph.middleEdgeOrientation.setComponent(c);
						//graph.middleEdgeOrientation.getPopup().show(e.getComponent(), e.getX(), e.getY());
						
					/*	if( c instanceof StateTransition){
							
							StateTransition st=(StateTransition)c;
							if(st.getMiddleEdge()!=null){
								if(st.getMiddleEdge() instanceof MiddleEdge){
									graph.setMiddleEdgeInstance((MiddleEdge)st.getMiddleEdge());
								}else if(st.getMiddleEdge() instanceof ReversibleMiddleEdge){
									graph.setMiddleEdgeInstance(((ReversibleMiddleEdge)st.getMiddleEdge()));
								}
							}		
							graph.middleEdgeOrientation.setComponent(c);
							graph.middleEdgeOrientation.getPopup().show(e.getComponent(), e.getX(), e.getY());
							
							st=null;
						}else if(c instanceof Association){
							Association as=(Association)c;
							if(as.getMiddleEdge()!=null){
								if(as.getMiddleEdge() instanceof MiddleEdgeAssociation){
									graph.setMiddleEdgeInstance((MiddleEdgeAssociation)as.getMiddleEdge());
								}
							}		
							graph.middleEdgeOrientation.setComponent(c);
							graph.middleEdgeOrientation.getPopup().show(e.getComponent(), e.getX(), e.getY());
							
						}*/
					}
					 c=null;
				}else{
					graph.refresh();
					graph.graphPopup.getPopup().show(e.getComponent(), e.getX(), e.getY());
					
				}
				
			 
			/*ONE CLICK ON SOMETHING ELSE THEN MOLECULES AND EDGES REFRESHES THE GRAPH*/
			}else{	
				graph.refresh();
				/*graph.refresh();
				BiochamMainFrame.frame.validate();
				BiochamMainFrame.frame.repaint();*/
				/*BiochamGraph.graphEditorFrame.repaint();
				BiochamGraphEditorDesktop.graphDesktop.repaint();*/
			
			}	
		}
		/*else{					
			graph.refresh();
		}	*/		
	}



	/**
	 * @param c
	 */
	public void editReaction(DefaultGraphCell c) {
		BiochamEdgeData edgeData=(BiochamEdgeData)c.getUserObject();
		String s = (String)JOptionPane.showInputDialog(
		        BiochamMainFrame.frame,
		        "Reaction kinetics",
		        "Kinetics",
		        JOptionPane.PLAIN_MESSAGE,
		        null,
		        null,
		        edgeData.getKinetics());

		if(s!=null && s!=""){									
				
				if(c instanceof StateTransition){
					if(!s.equals(edgeData.getKinetics())){
						StateTransition st=(StateTransition)c;
						edgeData.setKinetics(s);						
						st.getMIDDLEIntermediateVertex().setUserObject(edgeData);						
						String newName=editNotReversibleRaction(edgeData,s);
						edgeData.setName(newName);
						st.setId(newName);	
						((StateTransition)c).setId(newName);
						c.setUserObject(edgeData);
						st=null;
					}
					
				}else if(c instanceof Association){
					if(!s.equals(edgeData.getKinetics())){
						Association as=(Association)c;
						edgeData.setKinetics(s);
						c.setUserObject(edgeData);
						as.getMIDDLEIntermediateVertex().setUserObject(edgeData);						
						edgeData.setName(editNotReversibleRaction(edgeData,s));
						c.setUserObject(edgeData);
						as.setId(edgeData.getName());
						as=null;
					}
					
				}else if(c instanceof Dissociation){
					if(!s.equals(edgeData.getKinetics())){
						Dissociation dis=(Dissociation)c;
						edgeData.setKinetics(s);
						c.setUserObject(edgeData);
						dis.getMIDDLEIntermediateVertex().setUserObject(edgeData);						
						edgeData.setName(editNotReversibleRaction(edgeData,s));
						c.setUserObject(edgeData);
						dis.setId(edgeData.getName());
						dis=null;
					}
					
				}else{
					boolean reversibleDir=false;
					Object[] possibilities = {"forward reaction", "reverse reaction"};  					
					String res = (String)JOptionPane.showInputDialog(BiochamMainFrame.frame,
									" \nTo with reaction you want to modify its kinetics?\n\n",
									"Edit Reaction Kinetics",
									JOptionPane.PLAIN_MESSAGE,
									Icons.icons.get("question.png"),
									possibilities,
									"forward reaction");
					if(res!=null){
						if(res.equals("reverse reaction")){
							reversibleDir=true;
						}else{
							reversibleDir=false;
						}
						if(c instanceof ReversibleStateTransition){
							ReversibleStateTransition rst=(ReversibleStateTransition)c;
							//forward or backward
							if(reversibleDir){
								if(rst.getIntermediateVertexMIDDLE2().getUserObject() instanceof BiochamEdgeData){
									((BiochamEdgeData)rst.getIntermediateVertexMIDDLE2().getUserObject()).setKinetics(s);								
								}
							}else{
								if(rst.getIntermediateVertexMIDDLE1().getUserObject() instanceof BiochamEdgeData){
									((BiochamEdgeData)rst.getIntermediateVertexMIDDLE1().getUserObject()).setKinetics(s);
								}
							}
							edgeData.setName(editReversibleRaction(edgeData.getName(),s,reversibleDir));
							rst.setId(edgeData.getName());
							c.setUserObject(edgeData);
						}else if(c instanceof ReversibleAssociation){
							ReversibleAssociation ra=(ReversibleAssociation)c;
							//forward or backward
							/*if((ra.isReversibleDissociation() && reversibleDir) || (ra.isReversibleDissociation() && !reversibleDir)){
								reversibleDir=false;
							}*/
							if(reversibleDir){
								
								if(ra.getIntermediateVertexMIDDLE2().getUserObject() instanceof BiochamEdgeData){
									((BiochamEdgeData)ra.getIntermediateVertexMIDDLE2().getUserObject()).setKinetics(s);								
								}
							}else{
								if(ra.getIntermediateVertexMIDDLE1().getUserObject() instanceof BiochamEdgeData){
									((BiochamEdgeData)ra.getIntermediateVertexMIDDLE1().getUserObject()).setKinetics(s);
								}
							}
							if(ra.isReversibleDissociation()){
								edgeData.setName(editReversibleRaction(edgeData.getName(),s,!reversibleDir));
							}else{
								edgeData.setName(editReversibleRaction(edgeData.getName(),s,reversibleDir));	
							}
							
							ra.setId(edgeData.getName());
							c.setUserObject(edgeData);
						}
					}	
				}				
			}								
		}
	//s is kinetics
	private String editReversibleRaction(String odlRule, String kinetics, boolean reversible){								
		
		String newRule;
		String ruleToChange;
		String tmp;
		if(reversible){
			ruleToChange=odlRule.substring(odlRule.indexOf(",")+1,odlRule.length()-1);	
			if(ruleToChange.contains("for")){
				tmp=kinetics+" "+ruleToChange.substring(ruleToChange.indexOf("for"));
			}else{
				tmp=kinetics+" for "+ruleToChange;
			}
			newRule=odlRule.substring(0,odlRule.indexOf(",")+1)+tmp+"}";
		}else{
			ruleToChange=odlRule.substring(1,odlRule.indexOf(","));
			if(ruleToChange.contains("for")){
				tmp=kinetics+" "+ruleToChange.substring(ruleToChange.indexOf("for"));
			}else{
				tmp=kinetics+" for "+ruleToChange;
			}
			newRule="{"+tmp+odlRule.substring(odlRule.indexOf(","));
		}
		
		Object[] objs=graph.getContainingRules().get(odlRule);
		Object[] tmps=null;
		if(objs!=null){
			tmps=new Object[objs.length];
			for(int i=0;i<objs.length;i++){
				tmps[i]=objs[i];
			}
		}
		if(tmps!=null){
			graph.getContainingRules().remove(odlRule);
			graph.getContainingRules().put(newRule,tmps);
		}
		((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).setFromGraph(true);
		graph.setAllAdded(true);
		((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).changeRules(ruleToChange,tmp);
		graph.setAllAdded(true);
		((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).setFromGraph(true);	
		
		objs=null;
		tmp=null;
		tmps=null;
		ruleToChange=null;
		return newRule;
	}
	
	//s is kinetics
	private String editNotReversibleRaction(BiochamEdgeData edgeData, String s){
		String rule=edgeData.getName();								
		String newRule="";
		if(rule!=null && rule!=""){
			
			if(rule.contains("for")){
				newRule=s+" "+rule.substring(rule.indexOf("for"));
			}else{
				newRule=s+" for "+rule;
			}
			
			graph.updateReaction(rule, newRule);
			Object[] objs=graph.getContainingRules().get(rule);
			Object[] tmp=null;
			if(objs!=null){
				tmp=new Object[objs.length];
				for(int i=0;i<objs.length;i++){
					tmp[i]=objs[i];
				}
			}
			if(tmp!=null){
				graph.getContainingRules().remove(rule);
				graph.getContainingRules().put(newRule,tmp);
			}
			((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).setFromGraph(true);
			graph.setAllAdded(true);
			((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).changeRules(rule,newRule);
			graph.setAllAdded(true);
			((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).setFromGraph(true);	
			
			objs=null;
			tmp=null;
		}		
		return newRule;
	}

	/**
	 * @param c
	 */
	public void editBiochamEntity(DefaultGraphCell c) {
		BiochamEntityData before = (BiochamEntityData)c.getUserObject();
		//String ic=before.getInitialConcentration();
		//String name=before.getName();
		
		MoleculeEditorWindow modifyMol=new MoleculeEditorWindow(BiochamMainFrame.frame,before,graph);
		/*BiochamEntityData after=modifyMol.getMolData();
										
		if(!name.equals(after.getName()) || after.getInitialConcentration()!=before.getInitialConcentration()){
			
			String s="present("+after.getName()+","+after.getInitialConcentration()+").\n";
			graph.getBiochamModel().sendToBiocham(s,"initConc");
			Vector<Rule> rules=new Vector<Rule>();
		
			for(int p=0;p<((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).getRules().size();p++){
				rules.add(((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).getRules().get(p));
			}
			
			for(int i=0;i<rules.size();i++){
				
				String reaction=rules.get(i).getName();
				DefaultGraphCell r=GraphUtilities.getCellByName(graph,reaction);
				
				if(r!=null){
					BiochamEdgeData eData=(BiochamEdgeData)r.getUserObject();
					
					if(reaction.contains(name)){
						
						String reactionA=reaction.replace(name,after.getName());
						
						graph.updateReaction(reaction,reactionA);
						Object[] objs=graph.getContainingRules().get(reaction);
						Object[] tmp=null;
						if(objs!=null){
							tmp=new Object[objs.length];
							for(int k=0;k<objs.length;k++){
								tmp[k]=objs[k];
							}
						}
						if(tmp!=null){
							graph.getContainingRules().put(reactionA,tmp);
						}
																		
						((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).deleteRule(reaction, false,false);
						((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).setFromGraph(true);
						graph.setAllAdded(true);
						GraphUtilities.addReactionFromGraphToModel(graph,reactionA);
						graph.setAllAdded(true);
						((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).setFromGraph(true);
						eData.setName(reactionA);
						
						if(eData.getProducts()!=null){
								if(eData.getProducts().size()>0){
									DefaultGraphCell cell=GraphUtilities.getCellById(graph,eData.getProducts().get(0).getId());
									if(cell!=null){
										BiochamEntityData cData = (BiochamEntityData)cell.getUserObject();
										if(cData.getName().contains(name)){
											cData.setName(cData.getName().replace(name,after.getName()));
											cell.setUserObject(cData);															
										}
										cData=null;
									}
									cell=null;
								}
						}
						reactionA=null;
						objs=null; tmp=null;
					}
					eData=null;
				}
				reaction=null; r=null;
			}
			s=null; rules.clear();rules=null;
			
		}else if(ic!=after.getInitialConcentration()){
			if(((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).indexOf(name)>=0){// It exist this molecule....
				if(after.getInitialConcentration()!=((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).getValue(((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).indexOf(name))){									
					((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).modify(name,after.getInitialConcentration());
				}
			}else{// this molecule doesn't exist
				String s="present("+after.getName()+","+after.getInitialConcentration()+").\n";
				graph.getBiochamModel().sendToBiocham(s,"initConc");
				s=null;
			}
		}								
		c.setUserObject(after);						
			
		modifyMol=null; after=null;
		name=null; ic=null; before=null;*/
		modifyMol=null;
		before=null;
	}



	public void mouseEntered(MouseEvent e) {

		
	}



	public void mouseExited(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}



	public void mousePressed(MouseEvent e) {
		
		
	}



	public void mouseReleased(MouseEvent e) {
		
		graph.refresh();
		
	}
	
	
}
