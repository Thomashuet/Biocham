package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.dialogs.DialogAddSpecification;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import javax.swing.JCheckBox;
import javax.swing.JOptionPane;





/**
 * The Controller executes the corresponding methods on RulesView clicking events.
 * It updates the Rules model and the Rules view, and sends commands to Biocham.
 * 
 * */
public class RulesController implements ActionListener{

	
	
	RulesModel rulesModel;
	RulesView rulesView;
	ModifyRule modifyListener;
	DeleteRule deleteListener;
		
	/**
	 * Constructor saves references of the view and the model, and creates instances of rules view's event listeners.
	 * */
	public RulesController(RulesModel model, RulesView view){
		rulesModel=model;
		rulesView=view;				
		modifyListener=new ModifyRule();
		deleteListener=new DeleteRule();
		
	}
	
	
	
	/**
	 * Deletes all the model's rules. Sends the appropriate command to Biocham.
	 * */
	public void deleteAll() {	
		int asw=JOptionPane.showConfirmDialog(rulesView.getParentFrame(),"Are you sure you want to delete the current set of rules and all molecule declarations?","Confirm",JOptionPane.YES_NO_OPTION);
		if(asw==JOptionPane.YES_OPTION){	
			rulesModel.getBiochamModel().sendToBiocham("list_ODE.\n","kinetics");
			rulesModel.deleteAll();
			rulesModel.getBiochamModel().sendToBiocham("clear_rules.\n");
			rulesModel.getBiochamModel().sendToBiocham("list_molecules.\n");		
			//rulesView.refresh();
			/*rulesView.getCentralPanel().removeAll();
			rulesView.repaint();*/			
		}		
	}
	
	/**
	 * Deletes a rule from the panel. Sends the appropriate command to Biocham.
	 * 
	 * */
	public void deleteRule(String parent, boolean userInteraction) {
		
		if(userInteraction){
			
			/**
			 * If it's a reversible rule, ask which direction(s) user wants to delete.
			 * 
			 * */
			if(parent.contains("<=")){
				
				Object[] objs=new Object[1+rulesModel.getRules().get(parent).size()];
				objs[0]="Select which direction you want to delete:";
				for(int i=0; i<rulesModel.getRules().get(parent).size();i++){
					JCheckBox b=new JCheckBox(rulesModel.getRules().get(parent).get(i));
					b.setName(rulesModel.getRules().get(parent).get(i));
					objs[i+1]=b;
				}
				int answer=JOptionPane.showConfirmDialog(rulesView.getParentFrame(),objs,"Delete Rule",JOptionPane.OK_CANCEL_OPTION);
				if(answer==JOptionPane.OK_OPTION){
					ArrayList<String> rulesToAdd=new ArrayList<String>();
					for(int i=1;i<objs.length;i++){
						if(!((JCheckBox)objs[i]).isSelected()){
							rulesToAdd.add(((JCheckBox)objs[i]).getName());
						}
					}
					if(rulesToAdd.size()==0){
						//all selected
						deleteRuleFromBiocham(parent);
						
					}else if(rulesToAdd.size()==objs.length-1){
						//nothing selected
						//close
					}else{
						//some selected
						deleteRuleFromBiocham(parent);
						for(int i=0;i<rulesToAdd.size();i++){						
							addRuleToBiocham(rulesToAdd.get(i));
						}				
					}
				}
				
			}else{
				int answ=JOptionPane.showConfirmDialog(rulesView.getParentFrame(),"Are you sure you want to delete this reaction? ","Delete reaction rule",JOptionPane.YES_NO_OPTION);
				if(answ==JOptionPane.YES_OPTION){
					deleteRuleFromBiocham(parent);			
				}
			}
		}else{
			deleteRuleFromBiocham(parent);	
		}
		
		rulesView.refresh();
		
	}
	
	private void deleteRuleFromBiocham(String parent){
		
		StringBuilder b=new StringBuilder();
		b.append("delete_rules({");						
		for(int i=0;i<rulesModel.getRules().get(parent).size();i++){							
			b.append(rulesModel.getRules().get(parent).get(i));
			if(i<rulesModel.getRules().get(parent).size()-1){
				b.append(",");
			}
		}
		b.append("}).\n list_molecules.\n");
		rulesModel.getBiochamModel().sendToBiocham(b.toString(),"Rules");
		rulesModel.deleteRule(parent);						
		b=null;
	}
	
	private void addRuleToBiocham(String parent){
		
		StringBuilder b=new StringBuilder();
		b.append("add_rules(");
		b.append(parent);
		b.append(").\n");
		b.append("list_molecules.\n");
		rulesModel.getBiochamModel().sendToBiocham(b.toString(),"Rules");		
		b=null;
	}

	/**
	 * Modifies rule from the panel, in the way that it deletes the rule first, and after adds the modified version of it.
	 * Sends the appropriate commands to Biocham. 
	 * */
	public void modifyRule(String or,String nr) {		
		//delete+add
		deleteRule(or,false);
		addRuleToBiocham(nr);
		rulesView.refresh();
	}
	
	public void actionPerformed(ActionEvent e) {

		
		if(e.getActionCommand().equals("addRule")){
			
			DialogAddSpecification adds=new DialogAddSpecification(rulesView.getParentFrame(), rulesModel.getBiochamModel().getRules(), "Rules Operators",rulesView);
			String value=adds.getFormula();
			
			/**
			 * Send it to Biocham( The BiochamLoggetThread adds it after to the model, which notifies the view...)!
			 * 
			 * */
			if(value!=null){
			
				if(value.endsWith(".")){
					value=value.substring(0,value.lastIndexOf("."));
				}				
				if(value!=null){				
					addRuleToBiocham(value);
					//rulesView.refresh();
					
				}
			}			
			value=null;
			adds=null;			
			
			
		}else if(e.getActionCommand().equals("showKinetics")){
			
			rulesModel.getBiochamModel().sendToBiocham("list_ODE.\n","kinetics");
			rulesModel.getBiochamModel().getKineticsDialog().show(rulesModel.getBiochamModel());
			
		}else if(e.getActionCommand().equals("exportKinetics")){
			
			rulesModel.getBiochamModel().sendToBiocham("list_ODE.\n","kinetics");
			
			if(rulesModel.getBiochamModel().getKinetics()!=null){
				rulesModel.getBiochamModel().exportKinetics();
			}
			
		}else if(e.getActionCommand().equals("deleteAll")){
				deleteAll();	
		}else if(e.getActionCommand().equals("rge")){
			
			BiochamDynamicTree.workbench.replaceTabbedPane(rulesModel.getBiochamModel()).setVisible(true);
			
		}else if(e.getActionCommand().equals("rgraph")){
			rulesModel.getBiochamModel().sendToBiocham("draw_reactions.\n");
		}	
	}
	
	/**
	 * Listener that is responsible for modifying a particular reaction rule
	 * */
	class ModifyRule extends MouseAdapter{
		public void mouseClicked(MouseEvent e) {
			Component button=(Component) e.getSource();
			String name=button.getName();
			String s = (String)JOptionPane.showInputDialog(rulesView.getParentFrame(),"","Rule modification",JOptionPane.PLAIN_MESSAGE,null,null,name);			
			if ((s != null) && (s.length() > 0)) {			   
			   modifyRule(name,s);	   
			}			
			button=null;
			name=null;
			s=null;
		}
	}
	
	/**
	 * Listener that is responsible for deleting a particular reaction rule
	 * */
	class DeleteRule extends MouseAdapter{
		public void mouseClicked(MouseEvent e) {
			Component button=(Component) e.getSource();
			String name=button.getName();
			deleteRule(name,true);    
			button=null;
			name=null;
			
			
		}
	}	
	
	public ModifyRule getModifyListener() {
		return modifyListener;
	}
	public DeleteRule getDeleteListener() {
		return deleteListener;
	}



	public void setDeleteListener(DeleteRule deleteListener) {
		this.deleteListener = deleteListener;
	}
	
}
