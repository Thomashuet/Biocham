package fr.inria.contraintes.biocham;

import org.jdesktop.swingx.MultiSplitLayout;
import org.jdesktop.swingx.MultiSplitPane;

import fr.inria.contraintes.biocham.customComponents.DnDTabbedPane;
import fr.inria.contraintes.biocham.customComponents.UniversalScrollPane;
import fr.inria.contraintes.biocham.customComponents.CustomPlaf.MyPlafTabbedPane;
import fr.inria.contraintes.biocham.modelData.CTLView;
import fr.inria.contraintes.biocham.modelData.DeclarationsView;
import fr.inria.contraintes.biocham.modelData.EventsView;
import fr.inria.contraintes.biocham.modelData.InitialStateView;
import fr.inria.contraintes.biocham.modelData.InvariantsView;
import fr.inria.contraintes.biocham.modelData.MacrosView;
import fr.inria.contraintes.biocham.modelData.MoleculesView;
import fr.inria.contraintes.biocham.modelData.ParamTableConservationLaws;
import fr.inria.contraintes.biocham.modelData.ParamTableDeclarations;
import fr.inria.contraintes.biocham.modelData.ParamTableEvents;
import fr.inria.contraintes.biocham.modelData.ParamTableCTLSpecifications;
import fr.inria.contraintes.biocham.modelData.ParamTableInitConc;
import fr.inria.contraintes.biocham.modelData.ParamTableMacros;
import fr.inria.contraintes.biocham.modelData.ParamTableMolecules;
import fr.inria.contraintes.biocham.modelData.ParamTableParameters;
import fr.inria.contraintes.biocham.modelData.ParamTableRules;
import fr.inria.contraintes.biocham.modelData.ParamTableVolumes;
import fr.inria.contraintes.biocham.modelData.ParametersView;
import fr.inria.contraintes.biocham.modelData.RulesView;
import fr.inria.contraintes.biocham.modelData.VolumesView;
import fr.inria.contraintes.biocham.utils.Utils;
import java.awt.Component;
import java.util.ArrayList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;


/**
 * Class that represents the model editor for all the model's data components (like Rules, Parameters, Events, Macros, etc...).
 * 
 * @author Dragana Jovanovska  
 */ 
public class BiochamModelEditor extends JSplitPane{

	
	
	BiochamModel model;
	DnDTabbedPane tabbedPane;		
	
	
	public BiochamModelEditor(BiochamModel m){
	
		
		model=m;
		
		tabbedPane = new DnDTabbedPane();
		tabbedPane.setUI(new MyPlafTabbedPane());
		tabbedPane.setOpaque(true);
		tabbedPane.setName("tabbedPane");		
		tabbedPane.setBackground(Utils.backgroundColor);
		tabbedPane.setFocusable(false);
		tabbedPane.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);
		String layoutDef ="(COLUMN ";
		for(int i=0;i<model.getModelElements().length;i++){
			layoutDef+=model.getModelElements()[i].getShortElementName()+" ";
		}
		layoutDef+=")";			
		MultiSplitLayout.Node modelRoot = MultiSplitLayout.parseModel(layoutDef);
		MultiSplitPane multiSplitPane = new MultiSplitPane();
		multiSplitPane.setDividerSize(3);
		multiSplitPane.setContinuousLayout(true);	
		multiSplitPane.getMultiSplitLayout().setModel(modelRoot);		
		JPanel panel;
		for(int i=0;i<model.getModelElements().length;i++){
			panel=model.getModelElements()[i].getParametersPanel();    	 
	    	multiSplitPane.add(refreshScreen(panel, panel.getName()),model.getModelElements()[i].getShortElementName());
		}	
		super.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
		super.setLeftComponent(new UniversalScrollPane(multiSplitPane));
		super.setRightComponent(tabbedPane);		
		setResizeWeight(0.1);  
		setDividerLocation(100);
		
		panel=null;
		
	}
	
	
	public JScrollPane refreshScreen(JPanel panel,String elementName) {
		 
		Utils.debugMsg("MODEL EDITOR PANEL FLESHESSSSSS!!!!!!!!!!!!!!!!!!!!!!!");
		Component[] comps=panel.getComponents();
		ArrayList<Component> cs;
		if(elementName.equals("Parameters")){
			/*cs=new ArrayList<Component>();
		 
			for(int i=0;i<comps.length;i++){
				String s=comps[i].getName();
				if(!s.equals("Add") && !s.equals("refresh") && !s.equals("redAsterisk")){
					cs.add(comps[i]);
				}
			}
			panel.removeAll();
			panel=((ParamTableParameters)model.getParameters().getParamTable()).refreshPanel(cs);//model.refreshPanel(cs, panel);        				
			cs.clear();        				
			cs=null;
			return new JScrollPane(panel);*/
			ParametersView v=((ParamTableParameters)BiochamDynamicTree.currentModel.getParameters().getParamTable()).getView();//new ParametersView(BiochamMainFrame.frame,((ParamTableParameters)BiochamDynamicTree.currentModel.getParameters().getParamTable()).getParametersModel());
			return new JScrollPane(v);
		}else if(elementName.contains("Initial")){
			
			/*cs=new ArrayList<Component>();
			for(int i=0;i<comps.length;i++){
				if(!(comps[i].getName().equals("Add")) && !(comps[i].getName().equals("refresh"))){
					cs.add(comps[i]);
				}
			}
			panel.removeAll();
			panel=((ParamTableInitConc)model.getInitConditions().getParamTable()).refreshPanel(cs);				
			cs.clear();
			cs=null;	
			return new JScrollPane(panel);*/
			InitialStateView v=((ParamTableInitConc)BiochamDynamicTree.currentModel.getInitConditions().getParamTable()).getView();//new InitialStateView(BiochamMainFrame.frame,((ParamTableInitConc)BiochamDynamicTree.currentModel.getInitConditions().getParamTable()).getInitStateModel());
			return new JScrollPane(v);
			 
		}else if(elementName.equals("Molecules")){
			
			/*comps=model.getMolecules().getParametersPanel().getComponents(); 
			cs=new ArrayList<Component>();
			
			for(int i=0;i<comps.length;i++){
				if(!(comps[i] instanceof JButton) && !(comps[i].getName().equals("warning"))){ 						
					cs.add(comps[i]);
					
				}				
			}
			ArrayList<Component> cs2=new ArrayList<Component>();			
			JPanel panelVolumes=model.getVolumes().getParametersPanel();
			Component[] comps2=panelVolumes.getComponents();		
			for(int i=0;i<comps2.length;i++){
				if((comps2[i] instanceof JButton)){
					if(((JButton)comps2[i]).getActionCommand().equals("modify") || ((JButton)comps2[i]).getActionCommand().equals("delete") ){
						cs2.add(comps2[i]);
					}
				}else if((comps2[i] instanceof JLabel)){
					if(!comps2[i].getName().equals("refresh")){
						cs2.add(comps2[i]);
					}
				}else if(!(comps2[i] instanceof JButton)){
					cs2.add(comps2[i]);
				}
			}
			panel.removeAll();
			panelVolumes.removeAll();			
			panel=((ParamTableMolecules)model.getMolecules().getParamTable()).refreshPanel(cs);	
			panelVolumes=((ParamTableVolumes)model.getVolumes().getParamTable()).refreshPanel(cs2);
			cs.clear();
			cs2.clear();
			cs=null;
			cs2=null;
			JScrollPane pan2=new JScrollPane(panelVolumes);
		    return pan2;*/
			MoleculesView v=((ParamTableMolecules)BiochamDynamicTree.currentModel.getMolecules().getParamTable()).getView();//new MoleculesView(BiochamMainFrame.frame,((ParamTableMolecules)BiochamDynamicTree.currentModel.getMolecules().getParamTable()).getMoleculesModel());
			return new JScrollPane(v); 
		}else if(elementName.equals("Volumes")){
			
			/*comps=model.getMolecules().getParametersPanel().getComponents(); 
			cs=new ArrayList<Component>();
			
			for(int i=0;i<comps.length;i++){
				if(!(comps[i] instanceof JButton) && !(comps[i].getName().equals("warning"))){ 						
					cs.add(comps[i]);
					
				}				
			}
			ArrayList<Component> cs2=new ArrayList<Component>();			
			JPanel panelVolumes=model.getVolumes().getParametersPanel();
			Component[] comps2=panelVolumes.getComponents();		
			for(int i=0;i<comps2.length;i++){
				if((comps2[i] instanceof JButton)){
					if(((JButton)comps2[i]).getActionCommand().equals("modify") || ((JButton)comps2[i]).getActionCommand().equals("delete") ){
						cs2.add(comps2[i]);
					}
				}else if((comps2[i] instanceof JLabel)){
					if(!comps2[i].getName().equals("refresh")){
						cs2.add(comps2[i]);
					}
				}else if(!(comps2[i] instanceof JButton)){
					cs2.add(comps2[i]);
				}
			}
			panel.removeAll();
			panelVolumes.removeAll();			
			panel=((ParamTableMolecules)model.getMolecules().getParamTable()).refreshPanel(cs);	
			panelVolumes=((ParamTableVolumes)model.getVolumes().getParamTable()).refreshPanel(cs2);
			cs.clear();
			cs2.clear();
			cs=null;
			cs2=null;
			JScrollPane pan2=new JScrollPane(panelVolumes);
		    return pan2;*/
			VolumesView v=((ParamTableVolumes)BiochamDynamicTree.currentModel.getVolumes().getParamTable()).getView();//new VolumesView(BiochamMainFrame.frame,((ParamTableVolumes)BiochamDynamicTree.currentModel.getVolumes().getParamTable()).getVolumesModel());
			return new JScrollPane(v); 
		}else if(elementName.equals("Declarations")){
			
			/*cs=new ArrayList<Component>();
			for(int i=0;i<comps.length;i++){        					
			    	if((comps[i] instanceof JButton)){
						if(!((JButton)comps[i]).getActionCommand().equals("addDeclaration")){
							cs.add(comps[i]);
						}
					}else{
						cs.add(comps[i]);
					}
			    		
			}    					
			panel.removeAll();
			
			panel=((ParamTableDeclarations)model.getDeclarations().getParamTable()).refreshPanel(cs);		
			cs.clear();
			cs=null;				*/
			DeclarationsView v=((ParamTableDeclarations)BiochamDynamicTree.currentModel.getDeclarations().getParamTable()).getView();//new DeclarationsView(BiochamMainFrame.frame,((ParamTableDeclarations)BiochamDynamicTree.currentModel.getDeclarations().getParamTable()).getDeclarationsModel());
			return new JScrollPane(v);
			 
		}else if(elementName.equals("Macros")){
			
			/*cs=new ArrayList<Component>();
			for(int i=0;i<comps.length;i++){
				if((comps[i] instanceof JButton)){
					if(!((JButton)comps[i]).getActionCommand().equals("addMacro") && !((JButton)comps[i]).getActionCommand().equals("checkMacro")){
						cs.add(comps[i]);
					}
				}else{
					if(comps[i] instanceof JLabel){
						if(!(((JLabel)comps[i]).getName().equals("refresh"))){
							cs.add(comps[i]);
						}
						
					}else{
						cs.add(comps[i]);
					}
				}
			}
			panel.removeAll();
			
			panel=((ParamTableMacros)model.getMacros().getParamTable()).refreshPanel(cs);	
			cs.clear();
			cs=null;			
			return new JScrollPane(panel);*/
			MacrosView v=((ParamTableMacros)BiochamDynamicTree.currentModel.getMacros().getParamTable()).getView();//new MacrosView(BiochamMainFrame.frame,((ParamTableMacros)BiochamDynamicTree.currentModel.getMacros().getParamTable()).getMacrosModel());
			return new JScrollPane(v);
		}else if(elementName.equals("Events")){
			
			/*cs=new ArrayList<Component>();
			for(int i=0;i<comps.length;i++){
				if((comps[i] instanceof JButton)){
					if(!((JButton)comps[i]).getActionCommand().equals("addEvent")){
						cs.add(comps[i]);
					}
				}else{
					if(comps[i] instanceof JLabel){
						if(!(((JLabel)comps[i]).getName().equals("refresh"))){
							cs.add(comps[i]);
						}
						
					}else{
						cs.add(comps[i]);
					}
				}
			}
			panel.removeAll();
			
			panel=((ParamTableEvents)model.getEvents().getParamTable()).refreshPanel(cs);	
			cs.clear();
			cs=null;	
			return new JScrollPane(panel);*/
			EventsView v=((ParamTableEvents)BiochamDynamicTree.currentModel.getEvents().getParamTable()).getView();//new EventsView(BiochamMainFrame.frame,((ParamTableEvents)BiochamDynamicTree.currentModel.getEvents().getParamTable()).getEventsModel());
			return new JScrollPane(v);
		}else if(elementName.equals("Conservation Laws")){
			
			/*cs=new ArrayList<Component>();
			for(int i=0;i<comps.length;i++){
				if((comps[i] instanceof JButton)){
					if(!((JButton)comps[i]).getActionCommand().equals("addConservationLaw") && !((JButton)comps[i]).getActionCommand().equals("checkConservationLaw")){    								
						cs.add(comps[i]);
					}    													
				}else if(!(comps[i] instanceof JLabel)){
					cs.add(comps[i]);
				}
			}
			panel.removeAll();       				
			panel=((ParamTableConservationLaws)model.getConservationLaws().getParamTable()).refreshPanel(cs);        			
			cs.clear();        			
			cs=null;	
			JScrollPane slp1=new JScrollPane(panel);
			return slp1;*/
			InvariantsView v=((ParamTableConservationLaws)BiochamDynamicTree.currentModel.getConservationLaws().getParamTable()).getView();//new InvariantsView(BiochamMainFrame.frame,((ParamTableConservationLaws)BiochamDynamicTree.currentModel.getConservationLaws().getParamTable()).getInvariantsModel());
			return new JScrollPane(v);
		}/*else if(elementName.contains("Boolean")){
			CTLView v=new CTLView(((ParamTableCTLSpecifications)BiochamDynamicTree.currentModel.getCtlSpecifications().getParamTable()).getCtlModel());
			return new JScrollPane(v);
		}*/else if(elementName.contains("Reaction")){		
			
			/*
			 * 
			cs=new ArrayList<Component>();
			for(int i=0;i<comps.length;i++){
				if((comps[i] instanceof JButton)){
					if(!((JButton)comps[i]).getActionCommand().equals("addRule")){
						cs.add(comps[i]);
					}
				}else if((comps[i] instanceof JLabel)){
					if(!((JLabel)comps[i]).getName().equals("refresh")){
						cs.add(comps[i]);
					}
				}else{
					cs.add(comps[i]);
				}
			}
			panel.removeAll();
			
			panel=((ParamTableRules)model.getRules().getParamTable()).refreshPanel(cs);	
			cs.clear();
			cs=null;
			
			*
			*/
			RulesView v=((ParamTableRules)BiochamDynamicTree.currentModel.getRules().getParamTable()).getView();//new RulesView(BiochamMainFrame.frame,((ParamTableRules)BiochamDynamicTree.currentModel.getRules().getParamTable()).getRulesModel());
			return new JScrollPane(v);
	
	}else{
		return null;
	}

}



	public DnDTabbedPane getTabbedPane() {
		return tabbedPane;
	}



	public void setTabbedPane(DnDTabbedPane tabbedPane) {
		this.tabbedPane = tabbedPane;
	}
	 
	 
}
