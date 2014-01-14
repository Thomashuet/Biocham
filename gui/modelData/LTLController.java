package fr.inria.contraintes.biocham.modelData;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import fr.inria.contraintes.biocham.BiochamMainFrame;import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.dialogs.DialogAddSpecification;
import fr.inria.contraintes.biocham.dialogs.DialogOptions;
import fr.inria.contraintes.biocham.dialogs.DialogSearchParams;
import fr.inria.contraintes.biocham.dialogs.SatisfactionDegreeDialog;
import fr.inria.contraintes.biocham.utils.SupportedSuffixes;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;




/**
 * @author Dragana Jovanovska
 *
 */
public class LTLController implements ActionListener{
				
	
	
	LTLModel ltlModel;
	LTLView ltlView;	
	DeleteProperty deleteListener;	
	boolean revising=false;

	
	/**
	 * Constructor saves references of the view and the model, and creates instances of ltl specifications' view's event listeners.
	 * */
	public LTLController(LTLModel model, LTLView view){
		ltlModel=model;
		ltlView=view;			
		deleteListener=new DeleteProperty();	
	}

	public void addLTLProperty() {
	
		DialogAddSpecification params=new DialogAddSpecification(ltlView.getParentFrame(), ltlModel.getBiochamModel().getLtlSpecifications(), "LTL Operators",ltlView.topLeftPanel);
				
		String value=params.getFormula();
		if(value!=null && value!=""){
			ltlModel.getBiochamModel().sendToBiocham("add_ltl("+value+").\n","LTL");
		}
		
		value=null;
		
	}

	public void deleteLTLProperty(String s){
		ltlModel.deleteProperty(s);
		StringBuilder sb=new StringBuilder();
		sb.append("delete_ltl(");
		sb.append(s);		
		sb.append(").\n");
		ltlModel.getBiochamModel().sendToBiocham(sb.toString());
		sb=null;
		
	}
	
	public void saveLtlModelChecking() {
		final String text=ltlView.getTarea().getText();
		Utils.fileChooser.setDialogTitle("Save file as");
		Utils.fileChooser.setDialogType(JFileChooser.SAVE_DIALOG);
		Utils.fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		String rep=Utils.showSaveDialog("",ltlView.getParentFrame(),SupportedSuffixes.TEXT_SUFFIX);			
		if (rep!=null) {
	          
		   final String dir=( new File(rep)).getAbsolutePath();
		   SwingWorker sw=new SwingWorker(){

			@Override
			public Object construct() {
				try{
				    // Create file 
				    FileWriter fstream = new FileWriter(dir);
				    BufferedWriter out = new BufferedWriter(fstream);
				    out.write(text);
				    //Close the tarea stream
				    out.close();
				    }catch (Exception ex){//Catch exception if any
				    	JOptionPane.showMessageDialog(ltlView.getParentFrame(), ex.getMessage());
				      System.err.println("Error: " + ex.getMessage());
				    }				  
				return null;
			}

			@Override
			public void finished() {
				// TODO Auto-generated method stub
				
			}};
			sw.start();
		}
	}
	public void actionPerformed(ActionEvent e) {

		if(e.getSource() instanceof CustomToolTipButton){
			CustomToolTipButton b=(CustomToolTipButton)e.getSource();
			b.setBalloonToolTipVisible(false);
		}if(e.getActionCommand()=="addLTL"){			
			addLTLProperty();			
		}if(e.getActionCommand()=="CheckLTLSpec"){
			checkLTLCurrentSpec();
		}if(e.getActionCommand()=="CheckLTL"){			
			checkLTL();
		}if(e.getActionCommand()=="computeQFLTL"){			
			computeQFLTL();
		}if(e.getActionCommand()=="satisfactionDegree"){			
			satisfactionDegree();
		}if(e.getActionCommand()=="landscape"){			
			landscape();
		}if(e.getActionCommand()=="robustness"){			
			robustness();
		}if(e.getActionCommand()=="searchParams"){			
			searchParams();
		}if(e.getActionCommand()=="loadTrace"){			
			loadTrace();
		}if(e.getActionCommand().equals("saveLTLModelChecking")){
			saveLTLModelChecking();
		}
		
	}
	public void saveLTLModelChecking() {
		final String text=ltlView.getTarea().getText();
		Utils.fileChooser.setDialogTitle("Save file as");
		Utils.fileChooser.setDialogType(JFileChooser.SAVE_DIALOG);
		Utils.fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		String rep=Utils.showSaveDialog("",BiochamMainFrame.frame,SupportedSuffixes.TEXT_SUFFIX);			
		if (rep!=null) {
	          
		   final String dir=(new File(rep)).getAbsolutePath();
		   SwingWorker sw=new SwingWorker(){

			@Override
			public Object construct() {
				try{
				    // Create file 
				    FileWriter fstream = new FileWriter(dir);
				    BufferedWriter out = new BufferedWriter(fstream);
				    out.write(text);
				    //Close the tarea stream
				    out.close();
				    }catch (Exception ex){//Catch exception if any
				    	JOptionPane.showMessageDialog(BiochamMainFrame.frame, ex.getMessage());
				      System.err.println("Error: " + ex.getMessage());
				    }			
				return null;
			}
			@Override
			public void finished() {
				// TODO Auto-generated method stub				
			}
		   };
		   sw.start(); 		   	  
		}
	}
	
	void loadTrace(){
		Utils.fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		String rep=Utils.showOpenDialog(BiochamMainFrame.frame,SupportedSuffixes.CSV_SUFFIX);			
		if (rep!=null) {
			final File file=new File(rep);
			if(!file.isDirectory()){
	            String s = "load_trace('"+ file.getPath() +"').\n";
	            ltlModel.getBiochamModel().sendToBiocham(s,"LTL");
			}else{
				JOptionPane.showMessageDialog(BiochamMainFrame.frame,"Select a file, not a whole directory!", "Selection Warning",JOptionPane.WARNING_MESSAGE);
			}
		}
	}
	public void searchParams() {
		
		//ltlModel.setDontAskForModify(false);
		ltlModel.foundParameters.clear();
		ltlModel.setCmdFinished(false);
		ltlModel.setFound(false);
		
		DialogOptions od=new DialogOptions(ltlView.getParentFrame(),ltlModel.getBiochamModel().getModelElement("Numerical Temporal Properties"),"Search Parameters",null);
		String cmd=od.getNewOneParameter();
		DialogSearchParams spd=null;	
		if(cmd!=null){
			
			if(cmd.equals("search_parameters")){
				spd=new DialogSearchParams(ltlView.getParentFrame(),ltlModel.getBiochamModel().getModelElement("Numerical Temporal Properties"),"Search Parameters",null);						
			}else if(cmd.equals("search_all_parameters")){
				spd=new DialogSearchParams(ltlView.getParentFrame(),ltlModel.getBiochamModel().getModelElement("Numerical Temporal Properties"),"Search All Parameters",null);						
			}else if(cmd.equals("search_random_parameters")){
				spd=new DialogSearchParams(ltlView.getParentFrame(),ltlModel.getBiochamModel().getModelElement("Numerical Temporal Properties"),"Search Random Parameters",null);
			}else if(cmd.equals("search_random_all_parameters")){
				spd=new DialogSearchParams(ltlView.getParentFrame(),ltlModel.getBiochamModel().getModelElement("Numerical Temporal Properties"),"Search Random All Parameters",null);
			}else if(cmd.equals("search_parameters_cmaes")){
				spd=new DialogSearchParams(ltlView.getParentFrame(),ltlModel.getBiochamModel().getModelElement("Numerical Temporal Properties"),"Search Parameters CMAES",null);
			}else if(cmd.equals("robustness")){
				spd=new DialogSearchParams(ltlView.getParentFrame(),ltlModel.getBiochamModel().getModelElement("Numerical Temporal Properties"),"Robustness",null);						
			}else if(cmd.equals("landscape")){
				spd=new DialogSearchParams(ltlView.getParentFrame(),ltlModel.getBiochamModel().getModelElement("Numerical Temporal Properties"),"Landscape",null);			
			}else if(cmd.equals("cmaes_multi_conditions")){
				spd=new DialogSearchParams(ltlView.getParentFrame(),ltlModel.getBiochamModel().getModelElement("Numerical Temporal Properties"),"Search Parameters CMAES Multi Conditions",null);
			}	
			
			cmd=spd.getResult();
			ltlModel.setFromBeginning(true);
			if(cmd!=null){
				ltlModel.setStopPrinting(false);
				ltlView.appendResults("\n\n Searching started.............");		
				ltlModel.getBiochamModel().sendToBiocham(cmd,"LTL");
			}
		}
	}
	void satisfactionDegree(){
		SatisfactionDegreeDialog dg=new SatisfactionDegreeDialog(ltlView.getParentFrame(),ltlModel.getBiochamModel().getModelElement("Numerical Temporal Properties"));
		String cmd=dg.getCommand();
		if(cmd!=null){
			ltlView.appendResults("\n\n Computing satisfaction degree.............");
			ltlView.appendResults("\n\n It is not recommended that you start another task until this is not finished.\n If you want to stop this task, click on <Stop Execution> from the toolbar.\n\n");
			ltlModel.getBiochamModel().sendToBiocham(cmd,"LTL");
		}
		cmd=null;
		dg=null;
	}
	void robustness(){	
		DialogSearchParams spd=new DialogSearchParams(ltlView.getParentFrame(),ltlModel.getBiochamModel().getModelElement("Numerical Temporal Properties"),"Robustness",null);	
		String cmd=spd.getResult();
		//setCheck(false);
		ltlModel.setFromBeginning(true);
		if(cmd!=null){
			ltlView.appendResults("\n\n Computing robustness started.............");
			ltlView.appendResults("\n\n It is not recommended that you start another task until this is not finished.\n If you want to stop this task, click on <Stop Execution> from the toolbar.\n\n");
			ltlModel.getBiochamModel().sendToBiocham(cmd,"LTL");
		}
		cmd=null;
		spd=null;
	}
	void landscape(){
		
		DialogSearchParams spd=new DialogSearchParams(ltlView.getParentFrame(),ltlModel.getBiochamModel().getModelElement("Numerical Temporal Properties"),"Satisfaction Degree Landscape",null);		
		String cmd=spd.getResult();
		//ltlModel.setCheck(false);
		ltlModel.setFromBeginning(true);
		if(cmd!=null){
			ltlView.appendResults("\n\n Computing satisfaction degree landscape started.............");
			ltlView.appendResults("\n\n It is not recommended that you start another task until this is not finished.\n If you want to stop this task, click on <Stop Execution> from the toolbar.\n\n");
			ltlModel.getBiochamModel().sendToBiocham(cmd,"LTL");
		}
		cmd=null;
		spd=null;
	}
	void computeQFLTL() {
		
		DialogOptions od=new DialogOptions(ltlView.getParentFrame(),ltlModel.getBiochamModel().getModelElement("Numerical Temporal Properties"),"Compute QFLTL",null);
		String cmd="";
		Object[] newParam=new Object[2];
		newParam=od.getNewParameter();	
		String value=(String)newParam[1];
		od=null;
		newParam=null;	
		if(value!=null && value!="" && !value.equals(null) && value.length()>0){
			value=value.trim();
			cmd="domains("+value.trim()+").\n";			
			ltlModel.setFromBeginning(true);
			if(cmd!=null){
				ltlView.appendResults("\n\n Analyzing trace started.........");
				ltlView.appendResults("\nChecking the LTL property.........");
				ltlModel.getBiochamModel().sendToBiocham(cmd,"LTL");
			}
		}	
	}
	void checkLTL(){
		
		
		DialogOptions od=new DialogOptions(ltlView.getParentFrame(),ltlModel.getBiochamModel().getModelElement("Numerical Temporal Properties"),"Check LTL Property",null);
		String cmd="";
		Object[] newParam=new Object[2];
		newParam=od.getNewParameter();	
		String value=(String)newParam[1];
		od=null;
		newParam=null;	
		if(value!=null && value!="" && !value.equals(null) && value.length()>0){
			value=value.trim();
			cmd="check_ltl("+value.trim()+").\n";
			ltlModel.setFromBeginning(true);
			if(cmd!=null){
				ltlView.appendResults("\n\n Analyzing trace started.........");
				ltlView.appendResults("\nChecking the LTL property.........");
				ltlModel.getBiochamModel().sendToBiocham(cmd,"LTL");
			}
		}	
	}
	
	void checkLTLCurrentSpec(){				
		
		DialogOptions od=new DialogOptions(ltlView.getParentFrame(),ltlModel.getBiochamModel().getModelElement("Numerical Temporal Properties"),"Check LTL",null);
		String cmd="";
		Object[] newParam=new Object[2];
		newParam=od.getNewParameter();
		String name=(String)newParam[0];
		String value=(String)newParam[1];
		if(value!=null){
			od=null;
			newParam=null;	
			Utils.debugMsg("Value is: "+value);
			if(name!=null && name!="" && !name.equals(null) && name.length()>0){
				//model.createModelBackup();
				if(name.length()>0){
					cmd="check_ltl_spec("+name+").\n";
					ltlModel.getBiochamModel().simulationMap.put("checkLTL", name);
				}else{
					cmd="check_ltl_spec.\n";
				}
				ltlModel.setFromBeginning(true);
				//setCheck(true);
				if(cmd!=null){
					ltlView.appendResults("\n\n Checking LTL specifications.......\n");
					ltlModel.getBiochamModel().sendToBiocham(cmd,"LTL");
				}
			}else{
				cmd="check_ltl_spec.\n";
				ltlModel.setFromBeginning(true);
				//setCheck(true);
				if(cmd!=null){
					ltlView.appendResults("\n\n Checking LTL specifications.......\n");
					ltlModel.getBiochamModel().sendToBiocham(cmd,"LTL");
				}
			}
		}
	}
	
	
	public LTLModel getLTLModel() {
		return ltlModel;
	}

	public void setLTLModel(LTLModel ltlModel) {
		this.ltlModel = ltlModel;
	}

	public LTLView getLTLView() {
		return ltlView;
	}

	public void setLTLView(LTLView ltlView) {
		this.ltlView = ltlView;
	}

	public DeleteProperty getDeleteListener() {
		return deleteListener;
	}

	public void setDeleteListener(DeleteProperty deleteListener) {
		this.deleteListener = deleteListener;
	}
	
	
	/**
	 * Listener that is responsible for deleting a particular LTL Property
	 * */
	class DeleteProperty extends MouseAdapter{
		public void mouseClicked(MouseEvent e) {
			Component button=(Component) e.getSource();
			String name=button.getName();
			deleteLTLProperty(name); 
			button=null;
			name=null;
			
			
		}
	}	
}
