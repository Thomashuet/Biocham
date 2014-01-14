package fr.inria.contraintes.biocham.modelData;
import fr.inria.contraintes.biocham.BiochamModelElement;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.dialogs.CustomInputDialog;
import fr.inria.contraintes.biocham.dialogs.DialogAddSpecification;
import fr.inria.contraintes.biocham.dialogs.DialogOptions;
import fr.inria.contraintes.biocham.utils.SupportedSuffixes;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.util.ArrayList;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

/**
 * @author Dragana Jovanovska
 *
 */

public class CTLController implements ActionListener{
				
	
	
	CTLModel ctlModel;
	CTLView ctlView;	
	DeleteProperty deleteListener;	
	boolean revising=false;

	
	/**
	 * Constructor saves references of the view and the model, and creates instances of ctl specifications' view's event listeners.
	 * */
	public CTLController(CTLModel model, CTLView view){
		ctlModel=model;
		ctlView=view;			
		deleteListener=new DeleteProperty();	
	}

	
	
	
	
	public void actionPerformed(ActionEvent e) {
		
		if(e.getSource() instanceof CustomToolTipButton){
			CustomToolTipButton b=(CustomToolTipButton)e.getSource();
			b.setBalloonToolTipVisible(false);
		}if(e.getActionCommand()=="addCTL"){
			
			revising=false;
			addCTLProperty();
			
		}if(e.getActionCommand()=="reviseModel"){
			
			revising=true;
			ctlView.appendResults("\n\n");
			reviseModel();
		}if(e.getActionCommand().equals("saveCTLModelChecking")){
		
			revising=false;
			saveCTLModelChecking();
		}if(e.getActionCommand()=="checkCTL"){
			
			revising=false;
			ctlView.appendResults("\n\n");
			checkCTL();
		}if(e.getActionCommand()=="nusmv"){
			
			revising=false;
			ctlView.appendResults("\n\n");
			nusmv();
		}if(e.getActionCommand()=="reduceModel"){
			
			revising=false;
			ctlView.appendResults("\n\n");
			reduceModel();
		}if(e.getActionCommand()=="learnRules"){
			
			revising=false;
			ctlView.appendResults("\n\n");
			learnRules();
		
		}if(e.getActionCommand().equals("deleteParameters")){
			ctlModel.getBiochamModel().sendToBiocham("clear_spec.\n");
			ctlModel.getSpecifications().clear();		
			ctlView.refresh();
		
		}if(e.getActionCommand()=="addGenCTL"){
		
			ctlModel.setAddingGenSpec(true);
			SwingWorker sw1=new SwingWorker(){
				
				@Override
				public Object construct() {					
					ctlModel.getBiochamModel().sendToBiocham("add_genCTL.\n","CTL");
					
					return null;
				}

				@Override
				public void finished() {
					//ctlView.refresh();	 
					
				}
			};
			sw1.start();
		
		}if(e.getActionCommand()=="genCTL"){
			
			SwingWorker sw=new SwingWorker(){

				@Override
				public Object construct() {    					
					String s="genCTL.\n";    					
					ctlModel.getBiochamModel().sendToBiocham(s,"CTL");
					return null;
				}
				@Override
				public void finished() {}
			};
			sw.start();
		}
		
	}

	public void addCTLProperty() {
		//ctlModel.getBiochamModel().booleanTPF
		DialogAddSpecification params=new DialogAddSpecification(ctlView.getParentFrame(), ctlModel.getBiochamModel().getCtlSpecifications(), "CTL Operators",ctlView.topLeftPanel);
				
		String value=params.getFormula();
		if(value!=null && value!=""){
			ctlModel.getBiochamModel().sendToBiocham("add_spec("+value+").\n","CTL");
		}
		
		value=null;
		
	}

	public void deleteCTLProperty(String s){
		ctlModel.deleteProperty(s);
		StringBuilder sb=new StringBuilder();
		sb.append("delete_spec(");
		sb.append(s);		
		sb.append(").\n");
		ctlModel.getBiochamModel().sendToBiocham(sb.toString());
		sb=null;
		
	}
	
	public void reviseModel() {
		
		BiochamModelElement el=ctlModel.getBiochamModel().getModelElement("Boolean Temporal Properties");
		//el.getModel().createModelBackup();
		DialogOptions od=new DialogOptions(ctlView.getParentFrame(),el,"Revise Model",ctlView.topLeftPanel);
		String cmd="";
		Object[] newParam=new Object[2];
		newParam=od.getNewParameter();
		Object[] name=(Object[]) newParam[0];			
		String sec=(String)newParam[1];
		od.disposeObject();
		od=null;				
		newParam=null;
		if(name!=null && sec!=null){
			
			if(sec.equals("nothing")){
			
				if(name.length==0){
					cmd="revise_model.\n";					
					if(cmd!=null){
						ctlModel.getBiochamModel().sendToBiocham(cmd,"CTL");
					}
				}else{
					String s="{"+name[0];
					for(int i=1;i<name.length;i++){
						s+=","+name[i];
					}
					s+="}";
					cmd="revise_model("+s+").\n";				
					if(cmd!=null){
						ctlModel.getBiochamModel().sendToBiocham(cmd,"CTL");
					}
				}					
			}else if(sec.equals("selected")){
				if(name.length==0){
					cmd="revise_model_interactive.\n";
					
					if(cmd!=null){
						ctlModel.getBiochamModel().sendToBiocham(cmd,"CTL");
					}
				}else{
					String s="{"+name[0];
					for(int i=1;i<name.length;i++){
						s+=","+name[i];
					}
					s+="}";
					cmd="revise_model_interactive("+s+").\n";
					
					if(cmd!=null){
						ctlModel.getBiochamModel().sendToBiocham(cmd,"CTL");
					}
				}		
			}
			ctlView.appendResults(" Model revising started.........\n");
		}
	}

	public void saveCTLModelChecking() {
		final String text=ctlView.getTarea().getText();
		Utils.fileChooser.setDialogTitle("Save file as");
		Utils.fileChooser.setDialogType(JFileChooser.SAVE_DIALOG);
		Utils.fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		String rep=Utils.showSaveDialog("",ctlView.getParentFrame(),SupportedSuffixes.TEXT_SUFFIX);			
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
				    	JOptionPane.showMessageDialog(ctlView.getParentFrame(), ex.getMessage());
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

	public void checkCTL() {
		
		String[] possibilities = {"check", "check + why", "check all"};  
		
		String msg="<html>How do you want to test the adequacy of the model w.r.t. its specification?<br><br>"+
					"<b><u>Check :</u></b> For each unsatisfied CTL property computes the result of why.<br>"+
					"<b><u>Check + why :</u></b>   For each CTL property computes the result of why.<br>"+
					"<b><u>Check all :</u></b>          Summarizes the result with the first unsatisfied property if there is one.</html>";
		CustomInputDialog d=new CustomInputDialog(ctlView.getParentFrame(),"Check Model's CTL Specification", msg, false, "", "",possibilities);
		if(d.getInputVal()!=null && d.getInputVal().length()>0){
			   String cmd="";
			   boolean reordering=ctlView.topLeftPanel.reorderingOption.isSelected();
			   boolean fairness=ctlView.topLeftPanel.fairnessOption.isSelected();
			   boolean mode=ctlView.topLeftPanel.modeOption.isSelected();
			   boolean why=ctlView.topLeftPanel.whyOption.isSelected();//((ParamTableCTLSpecifications)model.getCtlSpecifications().getParamTable()).getWhyOption()
			   if(reordering){
				   cmd+="nusmv_dynamic_reordering.\n";
			   }else{
				   cmd+="nusmv_disable_dynamic_reordering.\n";
			   } 
			   if(fairness){
				   cmd+="fairness_path.\n";
			   }else{
				   cmd+="no_fairness_path.\n";
			   } 
			   if(mode){
				   cmd+="nusmv_direct.\n";
			   }else{
				   cmd+="nusmv_non_direct.\n";
			   }
			  /* if(why){
				   cmd+="check_why";
			   }else {
				   cmd+="check_ctl";
			   }*/
			   
			if(d.getInputVal().equals("check")){
				cmd+="check_spec.\n";
			}else if(d.getInputVal().equals("check + why")){
				cmd+="check_why_spec.\n";
			}else{
				cmd+="check_all_spec.\n";
			}
			if(cmd!=null){
				ctlView.appendResults(" Checking Model's CTL Specifications.........\n");
				ctlModel.getBiochamModel().sendToBiocham(cmd,"CTL");
			}
		}
	}

	public void nusmv() {
		
		BiochamModelElement el=ctlModel.getBiochamModel().getModelElement("Boolean Temporal Properties");
		//el.getModel().createModelBackup();
		DialogOptions od=new DialogOptions(ctlView.getParentFrame(),el,"Check CTL Property against the model",ctlView.topLeftPanel);
		String cmd="";
		Object[] newParam=new Object[2];
		newParam=od.getNewParameter();
		String name=(String)newParam[0];
		ArrayList<String> values=new ArrayList<String>();
		values=(ArrayList<String>)newParam[1];	
		if(name!=null && name.length()>0){					
			cmd=values.get(0)+values.get(1)+values.get(2)+values.get(3)+"("+name+").\n";			
			if(cmd!=null){
				ctlView.appendResults(" Model Checker NUSMV started............\n");
				ctlModel.getBiochamModel().sendToBiocham(cmd,"CTL");
			}
			values.clear();
			values=null;
		}else if(name==""){
			JOptionPane.showMessageDialog(ctlView.getParentFrame(), "You didn't specify a biocham query.");
		}
		od.disposeObject();
		od=null;				
		newParam=null;
	}

	public void reduceModel() {
		BiochamModelElement el=ctlModel.getBiochamModel().getModelElement("Boolean Temporal Properties");
		//el.getModel().createModelBackup();
		DialogOptions od=new DialogOptions(ctlView.getParentFrame(),el,"Reduce Model",ctlView.topLeftPanel);
		String cmd="";
		Object[] newParam=new Object[2];
		newParam=od.getNewParameter();
		Object[] name=(Object[]) newParam[0];			
		String sec=(String)newParam[1];
		od.disposeObject();
		od=null;				
		newParam=null;
		if(name!=null && sec!=null){					
			if(name.length==0){
				cmd="reduce_model.\n";
				if(cmd!=null){	
					ctlModel.getBiochamModel().sendToBiocham(cmd,"CTL");
				}
			}else{
				String s="{"+name[0];
				for(int i=1;i<name.length;i++){
					s+=","+name[i];
				}
				s+="}";
				cmd="reduce_model("+s+").\n";
				if(cmd!=null){
					ctlModel.getBiochamModel().sendToBiocham(cmd,"CTL");
				}
			}
			ctlView.appendResults(" Reducing model started.........\n");
		}
	}

	public void learnRules() {
		BiochamModelElement el=ctlModel.getBiochamModel().getModelElement("Boolean Temporal Properties");
		//el.getModel().createModelBackup();
		DialogOptions od=new DialogOptions(ctlView.getParentFrame(),el,"Learn Rules",ctlView.ctlSpecPanel);
		String cmd="";
		Object[] newParam=new Object[2];
		newParam=od.getNewParameter();		
		String choice=(String)newParam[1];
		if(choice!=null){
			if(choice.equals("addition")){
				
				String rule=(String) newParam[0];
				if(rule!=null && rule.length()>0){
					String s="{"+rule+"}";
					cmd="learn_one_addition("+s+").\n";
					if(cmd!=null){
						ctlView.appendResults(" Learning rules started.........\n");
						ctlModel.getBiochamModel().sendToBiocham(cmd,"CTL");
					}
				}else{
					JOptionPane.showMessageDialog(ctlView.getParentFrame(), "You must specify a rule to be added to the model.");
				}
			}else{
				
				Object[] name=(Object[]) newParam[0];
				if(name!=null){
					if(name.length==0){
						cmd="learn_one_deletion.\n";
					}else{
						String s="{"+name[0];
						for(int i=1;i<name.length;i++){
							s+=","+name[i];
						}
						s+="}";
						cmd="learn_one_deletion("+s+").\n";
					}
					if(cmd!=null){
						ctlView.appendResults(" Learning rules started.........\n");
						ctlModel.getBiochamModel().sendToBiocham(cmd,"CTL");
					}
				}
			}
		}
		od.disposeObject();
		od=null;				
		newParam=null;
	}

	public CTLModel getCTLModel() {
		return ctlModel;
	}

	public void setCTLModel(CTLModel ctlModel) {
		this.ctlModel = ctlModel;
	}

	public CTLView getCTLView() {
		return ctlView;
	}

	public void setCTLView(CTLView ctlView) {
		this.ctlView = ctlView;
	}

	public DeleteProperty getDeleteListener() {
		return deleteListener;
	}

	public void setDeleteListener(DeleteProperty deleteListener) {
		this.deleteListener = deleteListener;
	}
	
	
	
	
	
	/**
	 * Listener that is responsible for deleting a particular CTL Property
	 * */
	class DeleteProperty extends MouseAdapter{
		public void mouseClicked(MouseEvent e) {
			Component button=(Component) e.getSource();
			String name=button.getName();
			deleteCTLProperty(name); 
			button=null;
			name=null;
			
			
		}
	}	
		
}

