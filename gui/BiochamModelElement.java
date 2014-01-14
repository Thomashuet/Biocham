package fr.inria.contraintes.biocham;

import fr.inria.contraintes.biocham.customComponents.ParamTablePanel;
import fr.inria.contraintes.biocham.modelData.DeclarationsModel;
import fr.inria.contraintes.biocham.modelData.InitialStateModel;
import fr.inria.contraintes.biocham.modelData.InvariantsModel;
import fr.inria.contraintes.biocham.modelData.MacrosModel;
import fr.inria.contraintes.biocham.modelData.ModelData;
import fr.inria.contraintes.biocham.modelData.MoleculesModel;
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
import fr.inria.contraintes.biocham.modelData.Parameters;
import fr.inria.contraintes.biocham.modelData.ParametersModel;
import fr.inria.contraintes.biocham.modelData.RulesModel;
import fr.inria.contraintes.biocham.modelData.VolumesModel;
import fr.inria.contraintes.biocham.utils.SupportedSuffixes;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.Dimension;
import java.io.File;
import java.util.ArrayList;

import javax.swing.BorderFactory;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTree;
import javax.swing.SpringLayout;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;


/**
 * Class that represents a biocham model component, like Parameters, Rules, Events, Macros, etc.
 * 
 * @author Dragana Jovanovska  
 */ 
public class BiochamModelElement {
	
	
	
	 private ParamTablePanel parametersPanel;
	 private Parameters paramTable;
	 private String elementName;
	 private BiochamModel model;	
	 private ArrayList<BiochamDynamicTree> dtree;//list of rules........	
	 public String cfVariables="";
	 public String cfValues="";
	 BiochamModelEditor modelEditor;
	 ModelData modelData;
	
	





	public BiochamModelElement(DefaultMutableTreeNode parent,WorkbenchArea workbench, String elementName){
		 
		 parametersPanel=new ParamTablePanel(elementName);			
		
		 parametersPanel.setLayout(new SpringLayout());	
		 if(elementName.contains("Reaction Rules")){
			 dtree=new ArrayList<BiochamDynamicTree>();
		 }		 
		 setElementName(elementName);		
		 Object mm=parent.getUserObject();
		 BiochamModel model;
		 if(mm instanceof BiochamModel){
			 model=(BiochamModel)mm;
		 }else{
			 model=((BiochamDynamicTreeSeparator)mm).getModel();		 
		 }	 		 
		 setModel(model);
		 Parameters paramTable = null;
		 if(elementName.equals("Molecules")){
			 paramTable=new ParamTableMolecules(model,workbench,parametersPanel);	
			 //modelData=new MoleculesModel(model);
		 }
		 else if(elementName.equals("Declarations")){
			 paramTable=new ParamTableDeclarations(model,workbench,parametersPanel);
			// modelData=new DeclarationsModel(model);
		 }
		 else if(elementName.equals("Parameters")){
			 paramTable=new ParamTableParameters(model,workbench,parametersPanel);
			// modelData=new ParametersModel(model);
		 }
		 else if(elementName.contains("Initial")){
			 paramTable=new ParamTableInitConc(model,workbench,parametersPanel);
			// modelData=new InitialStateModel(model);
		 }
		 else if(elementName.contains("Reaction Rules")){
			 paramTable=new ParamTableRules(model,workbench,parametersPanel);
			// modelData=new RulesModel(model);
		 }
		 else if(elementName.equals("Events")){
			 paramTable=new ParamTableEvents(model,workbench,parametersPanel);
			// modelData=new MoleculesModel(model);
		 }
		 else if(elementName.equals("Macros")){
			 paramTable=new ParamTableMacros(model,workbench,parametersPanel);
			// modelData=new MacrosModel(model);
		 }
		 else if(elementName.equals("Boolean Temporal Properties")){
			// parametersPanel.setBackground(Utils.backgroundColor);
			 paramTable=new ParamTableCTLSpecifications(model,workbench,parametersPanel);
		 }
		 else if(elementName.equals("Numerical Temporal Properties")){
			// parametersPanel.setBackground(Utils.backgroundColor);
			 paramTable=new ParamTableLTLSpecifications(model,workbench,parametersPanel);
		 }
		 else if(elementName.equals("Volumes")){
			 paramTable=new ParamTableVolumes(model,workbench,parametersPanel);
			// modelData=new VolumesModel(model);
		 }
		 else if(elementName.equals("Conservation Laws")){
			 paramTable=new ParamTableConservationLaws(model,workbench,parametersPanel);
			 //modelData=new InvariantsModel(model);
		 }
		 setParamTable(paramTable);
		 setModelData(modelData);
		 if(!elementName.equals("Volumes")){// && !elementName.equals("LTLSpecifications")){
			 if(elementName.equals("Reaction Rule Model") || elementName.equals("Boolean Temporal Properties") || elementName.equals("Numerical Temporal Properties")){// || elementName.contains("Reaction Graph Editor")){ //!elementName.equals("Model Data")){
				 BiochamDynamicTree.treeListener.addNodeObject(parent,this);
				 Utils.debugMsg(parent.getChildCount());				
			 }
		 }		 	
	 }
	 
	 
	

	
	public void saveToFile(){		
		
		// IT SAVES UNDER THE SAME NAME..?Is it correct???
		String cmd=null;
		File file,newFile=null;
				
		
	//	boolean b=SwingUtilities.isEventDispatchThread();
		
		
		if(elementName.equals("Parameters") || elementName.contains("Initial")){
			if(getModel().isEnableExport()){
				
				String newFilename=Utils.showSaveDialog(null, BiochamMainFrame.frame,SupportedSuffixes.BIOCHAM_SUFFIX);
				if(newFilename!=null && newFilename!=""){
					newFile=new File(newFilename);
									 
					if(elementName.equals("Parameters")){
						cmd="export_param('"+newFile+"').\n";
						getModel().getBiochamInput().print(cmd);
					}else if(elementName.contains("Initial")){
						cmd="export_init('"+newFile+"').\n";
						getModel().getBiochamInput().print(cmd);
					}else{
						JOptionPane.showMessageDialog(BiochamMainFrame.frame, "You can save to file only the parameters and the initial concetrations of the model!");
					}		
				}
			}else{
				JOptionPane.showMessageDialog(BiochamMainFrame.frame, "First you have to save the model to file.");
			}  
		}else{
			JOptionPane.showMessageDialog(BiochamMainFrame.frame, "You can save to file only the parameters and the initial concetrations of the model!");
		}
		
	}

	
	
	
	

	public void exportElementAs(String s) {
		
		// 
		String cmd=null;
		//if(!model.isEnableExport()){
		//	JOptionPane.showMessageDialog(BiochamMainFrame.frame, "First you have to save this model.");
		//}else{
			//File f=model.getModelFile();
			//String name=getInitialFilenameFrom(f);
		
			String name=null;
			if(getElementName().equals("Parameters") || getElementName().contains("Initial")){
				
				name=Utils.showSaveDialog(null, BiochamMainFrame.frame,SupportedSuffixes.BIOCHAM_SUFFIX);
				
				if(name!=null && name!=""){
					
					File file=new File(name);
					
					if(getElementName().equals("Parameters")){
						cmd="export_param('"+file+"').\n";
					}else{
						cmd="export_init('"+file+"').\n";
					}
					
					boolean ok=true;					
					if(s.equals("dot")){
						cmd+="export_dot('"+file+"').\n";			
					}else if(s.equals("ode")){
						cmd+="export_ode('"+file+"').\n";
					}else if(s.equals("bc")){
						cmd+="export_biocham('"+file+"').\n";
					}else if(s.equals("latex")){
						cmd+="export_ode_latex('"+file+"').\n";
					}else if(s.equals("nusmv")){
						cmd+="export_nusmv('"+file+"').\n";
					}else if(s.equals("sbml")){
						cmd+="export_sbml('"+file+".xml"+"').\n";
					}else if(s.equals("lotos")){
						cmd+="export_lotos('"+file+"').\n";
					}else if(s.equals("prolog")){		
						cmd+="export_prolog('"+file+".pl"+"').\n";
					}else if(s.equals("slp")){
						cmd+="export_slp('"+file+".slp"+"').\n";
					}else{
						ok=false;
					}
					if(ok){
						model.getBiochamInput().print(cmd);
					}				
				}
				
			}else{
				JOptionPane.showMessageDialog(BiochamMainFrame.frame, "You can only save and export the Parameters and the Initial Concentrations.");				
			}
	}





	public void resetAll() {}
	public void deleteAll() {}
	public void print() {}

	
	public BiochamDynamicTree getDtree(String name) {
		
		
		for(int i=0;i<this.dtree.size();i++){
			if(dtree.get(i).getName().equals(name)){
				return dtree.get(i);
			}
		}return null;
		
	}
	public String toString() {		
        return getElementName();
	}	
	public synchronized ParamTablePanel getParametersPanel() {
		return parametersPanel;
	}
	public void setParametersPanel(ParamTablePanel parametersPanel) {
		this.parametersPanel = parametersPanel;
	}
	public Parameters getParamTable() {
		return this.paramTable;
	}
	public void setParamTable(Parameters paramTable) {
		this.paramTable = paramTable;
	}
	public String getElementName() {
		return elementName;
	}
	public String getShortElementName() {
		return elementName.replaceAll(" ", "");
	}
	public void setElementName(String elementName) {
		this.elementName = elementName;
	}	
	public BiochamModel getModel() {
		return model;
	}
	public void setModel(BiochamModel model) {
		this.model = model;
	}
		
	public void addDtree(BiochamDynamicTree dtree) {
		this.dtree.add(dtree);		
	}
	public void removeNodeDtree(int i) {
		this.dtree.remove(i);		
	}
	public int getNodeIndex(String name){

		
		for(int i=0;i<this.dtree.size();i++){
			if(dtree.get(i).getName().equals(name)){
				return i;
			}
		}
		return -1;
	
	}
	public ArrayList<BiochamDynamicTree> getDtreeList() {
		return dtree;
	}
	public void disposeElements() {
		

		 parametersPanel=null;
		// paramTable.disposeElements();
		 modelData=null;
		 paramTable=null;
		 elementName=null;
		 model=null;		 
		 if(dtree!=null){
			 dtree.clear();
		 }
		 dtree=null;
		
	}

	public void setCfVariables(String cfVariables) {
		this.cfVariables=cfVariables;
		
	}
	public String getCfVariables() {
		return cfVariables;
	}


	public String getCfValues() {
		return cfValues;
	}


	public void setCfValues(String cfValues) {
		this.cfValues = cfValues;
	}
	
	
	
	
	 public ModelData getModelData() {
			return modelData;
		}





		public void setModelData(ModelData modelData) {
			this.modelData = modelData;
		}
	
	
	
	
	
	
	
	
	
	

}
