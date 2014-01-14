package fr.inria.contraintes.biocham.customComponents;



import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JViewport;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.modelData.AbstractionView;
import fr.inria.contraintes.biocham.modelData.CTLView;
import fr.inria.contraintes.biocham.modelData.DeclarationsView;
import fr.inria.contraintes.biocham.modelData.EventsView;
import fr.inria.contraintes.biocham.modelData.InitialStateView;
import fr.inria.contraintes.biocham.modelData.InvariantsView;
import fr.inria.contraintes.biocham.modelData.LTLView;
import fr.inria.contraintes.biocham.modelData.MacrosView;
import fr.inria.contraintes.biocham.modelData.MoleculesView;
import fr.inria.contraintes.biocham.modelData.ParamTableCTLSpecifications;
import fr.inria.contraintes.biocham.modelData.ParamTableConservationLaws;
import fr.inria.contraintes.biocham.modelData.ParamTableDeclarations;
import fr.inria.contraintes.biocham.modelData.ParamTableEvents;
import fr.inria.contraintes.biocham.modelData.ParamTableInitConc;
import fr.inria.contraintes.biocham.modelData.ParamTableLTLSpecifications;
import fr.inria.contraintes.biocham.modelData.ParamTableMacros;
import fr.inria.contraintes.biocham.modelData.ParamTableMolecules;
import fr.inria.contraintes.biocham.modelData.ParamTableParameters;
import fr.inria.contraintes.biocham.modelData.ParamTableRules;
import fr.inria.contraintes.biocham.modelData.ParamTableVolumes;
import fr.inria.contraintes.biocham.modelData.ParametersView;
import fr.inria.contraintes.biocham.modelData.RulesView;
import fr.inria.contraintes.biocham.modelData.SimulationView;
import fr.inria.contraintes.biocham.modelData.VolumesView;
import fr.inria.contraintes.biocham.utils.Icons;



public class BiochamFrame extends JFrame{

	BiochamFrame instance;
	/**
	 * Custom frame used to give the same look to all the model popups generated from the menu.
	 * 
	 * @author Dragana Jovanovska  
	 */ 
	public BiochamFrame(final String t){
		super(t);
		instance=this;
		setIconImage(Icons.images.get("SSicon.png"));	
		setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);	  
		addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent e) {
            	if(t.contains("Boolean")){
            		JViewport vp=(JViewport)((JScrollPane)instance.getContentPane().getComponent(0)).getComponent(0);
            		CTLView v=(CTLView)vp.getComponent(0);             			
            		((ParamTableCTLSpecifications)BiochamDynamicTree.currentModel.getCtlSpecifications().getParamTable()).getCtlModel().getViews().remove(v);
            		v=null;
            		vp=null;
            	}else if(t.contains("Numerical")){
            		JViewport vp=(JViewport)((JScrollPane)instance.getContentPane().getComponent(0)).getComponent(0);
            		LTLView v=(LTLView)vp.getComponent(0);            	   		
            		((ParamTableLTLSpecifications)BiochamDynamicTree.currentModel.getLtlSpecifications().getParamTable()).getLtlModel().getViews().remove(v);
            		v=null;
            		vp=null;
            	}else if(t.contains("Simulation") && !t.contains("Plots")){
            		JViewport vp=(JViewport)((JScrollPane)instance.getContentPane().getComponent(0)).getComponent(0);
            		SimulationView v=(SimulationView)vp.getComponent(0);             		            		
            		BiochamDynamicTree.currentModel.getSimulationModel().getViews().remove(v);
            		v=null;
            		vp=null;
            	}else if(t.contains("Initial")){
            		JViewport vp=(JViewport)((JScrollPane)instance.getContentPane().getComponent(0)).getComponent(0);
            		InitialStateView v=(InitialStateView)vp.getComponent(0);             		          		
            		((ParamTableInitConc)BiochamDynamicTree.currentModel.getInitConditions().getParamTable()).getInitStateModel().getViews().remove(v);
            		v=null;
            		vp=null;
            	}else if(t.contains("Rules")){            	
            		JViewport vp=(JViewport)((JScrollPane)instance.getContentPane().getComponent(0)).getComponent(0);
            		RulesView v=(RulesView)vp.getComponent(0);         		
            		((ParamTableRules)BiochamDynamicTree.currentModel.getRules().getParamTable()).getRulesModel().getViews().remove(v);
            		v=null;
            		vp=null;
            	}else if(t.contains("Conservation")){
            		JViewport vp=(JViewport)((JScrollPane)instance.getContentPane().getComponent(0)).getComponent(0);
            		InvariantsView v=(InvariantsView)vp.getComponent(0);             	            		
            		((ParamTableConservationLaws)BiochamDynamicTree.currentModel.getConservationLaws().getParamTable()).getInvariantsModel().getViews().remove(v);
            		v=null;
            		vp=null;
            	}else if(t.contains("Parameters")){
            		JViewport vp=(JViewport)((JScrollPane)instance.getContentPane().getComponent(0)).getComponent(0);
            		ParametersView v=(ParametersView)vp.getComponent(0);             		            		
            		((ParamTableParameters)BiochamDynamicTree.currentModel.getParameters().getParamTable()).getParametersModel().getViews().remove(v);
            		v=null;
            		vp=null;
            	}else if(t.contains("Events")){
            		JViewport vp=(JViewport)((JScrollPane)instance.getContentPane().getComponent(0)).getComponent(0);
            		EventsView v=(EventsView)vp.getComponent(0);             		    		
            		((ParamTableEvents)BiochamDynamicTree.currentModel.getEvents().getParamTable()).getEventsModel().getViews().remove(v);
            		v=null;
            		vp=null;
            	}else if(t.contains("Macros")){
            		JViewport vp=(JViewport)((JScrollPane)instance.getContentPane().getComponent(0)).getComponent(0);
            		MacrosView v=(MacrosView)vp.getComponent(0);             	            		
            		((ParamTableMacros)BiochamDynamicTree.currentModel.getMacros().getParamTable()).getMacrosModel().getViews().remove(v);
            		v=null;
            		vp=null;
            	}else if(t.contains("Declarations")){
            		JViewport vp=(JViewport)((JScrollPane)instance.getContentPane().getComponent(0)).getComponent(0);
            		DeclarationsView v=(DeclarationsView)vp.getComponent(0);               		            		
            		((ParamTableDeclarations)BiochamDynamicTree.currentModel.getDeclarations().getParamTable()).getDeclarationsModel().getViews().remove(v);
            		v=null;
            		vp=null;
            	}else if(t.contains("Volumes")){
            		JViewport vp=(JViewport)((JScrollPane)instance.getContentPane().getComponent(0)).getComponent(0);
            		VolumesView v=(VolumesView)vp.getComponent(0);               		            		
            		((ParamTableVolumes)BiochamDynamicTree.currentModel.getVolumes().getParamTable()).getVolumesModel().getViews().remove(v);
            		v=null;
            		vp=null;
            	}else if(t.contains("Molecules")){
            		JViewport vp=(JViewport)((JScrollPane)instance.getContentPane().getComponent(0)).getComponent(0);
            		MoleculesView v=(MoleculesView)vp.getComponent(0);                	            		
            		((ParamTableMolecules)BiochamDynamicTree.currentModel.getMolecules().getParamTable()).getMoleculesModel().getViews().remove(v);
            		v=null;
            		vp=null;
            	}else if(t.contains("Abstractions")){
            		JViewport vp=(JViewport)((JScrollPane)instance.getContentPane().getComponent(0)).getComponent(0);
            		AbstractionView v=(AbstractionView)vp.getComponent(0);                		            		
            		BiochamDynamicTree.currentModel.getAbstractionModel().getViews().remove(v);
            		v=null;
            		vp=null;
            	}else if(t.contains("Simulation Plots Comparison")){
            		BiochamDynamicTree.compWindowAlreadyOpened=false;
            	}                           	
            	((JFrame)e.getSource()).setVisible(false);	            
            }
        });
	}
}
