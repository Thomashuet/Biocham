package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.dialogs.CustomInputDialog;
import fr.inria.contraintes.biocham.dialogs.DialogAddSpecification;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;


/**
 * The Controller executes the corresponding methods on InvariantsView clicking events.
 * It updates the invariants model and the invariants view, and sends commands to Biocham.
 * 
 * */
public class InvariantsController implements ActionListener{

	InvariantsModel invariantsModel;
	InvariantsView invariantsView;	
	DeleteInvariant deleteListener;
	
	/**
	 * Constructor saves references of the view and the model, and creates instances of rules view's event listeners.
	 * */
	public InvariantsController(InvariantsModel model, InvariantsView view){
		invariantsModel=model;
		invariantsView=view;			
		deleteListener=new DeleteInvariant();
	}

	public void actionPerformed(ActionEvent e) {
		if(e.getActionCommand().equals("addInvariant")){
			add();
		}else if(e.getActionCommand().equals("checkConservationLaw")){
			check();
		}else if(e.getActionCommand().equals("P-invariants")){
			compute();
		}
		
		
	}

	public void check(){		
		invariantsModel.getBiochamModel().getModelEditor().getTabbedPane().removeAll();
		((ParamTableConservationLaws)invariantsModel.getBiochamModel().getConservationLaws().getParamTable()).checkConservationLaw();
		invariantsModel.getBiochamModel().getModelEditor().getTabbedPane().addTab("Invariants-Output", new JScrollPane(((ParamTableConservationLaws)invariantsModel.getBiochamModel().getConservationLaws().getParamTable()).getCheckingPanel()));
		BiochamDynamicTree.setFocusOnMEditor();
	}
	
	
	public void compute(){
		invariantsModel.getBiochamModel().getModelEditor().getTabbedPane().removeAll();
		String msg="<html>Enter a limit on the highest value<br>"+
		"found in a given P-invariant (the default is 4):</html>";
		CustomInputDialog d=new CustomInputDialog(invariantsView.getParentFrame(),"P-Invariant limit", msg, true, "4", "",null);
		int k=-1;
		if(d.getInputVal()!=null && d.getInputVal().length()>0){
			if(!d.getInputVal().equals("4")){
				try{
					k=Integer.parseInt(d.getInputVal());					
				}catch(NumberFormatException ex){
					JOptionPane.showMessageDialog(invariantsView.getParentFrame(), "The input should be an Integer value.","Error",JOptionPane.ERROR_MESSAGE);
				}
			}else{
				k=4;
			}
		}
		if(k>-1){
			((ParamTableConservationLaws)invariantsModel.getBiochamModel().getConservationLaws().getParamTable()).computePinvariants(k);
			invariantsModel.getBiochamModel().getModelEditor().getTabbedPane().addTab("Invariants-Output", new JScrollPane(((ParamTableConservationLaws)invariantsModel.getBiochamModel().getConservationLaws().getParamTable()).getCheckingPanel()));	
		}
		BiochamDynamicTree.setFocusOnMEditor();
	}
	
	public void add() {
		
		DialogAddSpecification params=new DialogAddSpecification(invariantsView.getParentFrame(), invariantsModel.getBiochamModel().getConservationLaws(), "Conservation Laws",null);		
		String value=params.getFormula();
		if(value!=null && value!=""){
			//invariantsModel.addInvariant(value);
				
			StringBuilder sb=new StringBuilder();
			sb.append("conservation(");
			sb.append(value);
			sb.append(").\n");
			invariantsModel.getBiochamModel().sendToBiocham(sb.toString());
			sb=null;			
		}
		
	}
	public void delete(String s){
		invariantsModel.deleteInvariant(s);
		invariantsView.refresh();	
		StringBuilder sb=new StringBuilder();
		sb.append("delete_conservation(");
		sb.append(s);		
		sb.append(").\n");
		invariantsModel.getBiochamModel().sendToBiocham(sb.toString());
		sb=null;
		
	}

	public InvariantsModel getInvariantsModel() {
		return invariantsModel;
	}

	public void setInvariantsModel(InvariantsModel invariantsModel) {
		this.invariantsModel = invariantsModel;
	}

	public InvariantsView getInvariantsView() {
		return invariantsView;
	}

	public void setInvariantsView(InvariantsView invariantsView) {
		this.invariantsView = invariantsView;
	}

	public DeleteInvariant getDeleteListener() {
		return deleteListener;
	}

	public void setDeleteListener(DeleteInvariant deleteListener) {
		this.deleteListener = deleteListener;
	}
	
	/**
	 * Listener that is responsible for deleting a particular Invariant
	 * */
	class DeleteInvariant extends MouseAdapter{
		public void mouseClicked(MouseEvent e) {
			Component button=(Component) e.getSource();
			String name=button.getName();
			delete(name);   
			button=null;
			name=null;
			
			
		}
	}	
}

