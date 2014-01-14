package fr.inria.contraintes.biocham.modelData;


import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.WorkbenchToolBars;
import fr.inria.contraintes.biocham.customComponents.ModifyButton;
import fr.inria.contraintes.biocham.dialogs.DialogAddParameter;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.JFormattedTextField;




/**
 * The Controller executes the corresponding methods on ParameterView clicking events.
 * It updates the Parameters model and the Parameters view, and sends commands to Biocham.
 * 
 * */
public class ParametersController implements ActionListener{

	ParametersModel parametersModel;
	ParametersView parametersView;
	ParametersListener mouseListener;
	DeleteParameter deleteListener;
	ParameterKeyListener keyListener;
	/**
	 * Constructor saves references of the view and the model, and creates instances of rules view's event listeners.
	 * */
	public ParametersController(ParametersModel model, ParametersView view){
		parametersModel=model;
		parametersView=view;				
		mouseListener=new ParametersListener();
		keyListener=new ParameterKeyListener();
		deleteListener=new DeleteParameter();
	}

	public void actionPerformed(ActionEvent e) {
		if(e.getActionCommand().equals("addParameter")){
			add();
		}
		
	}
	public void add() {
		
		DialogAddParameter param=new DialogAddParameter(parametersView.getParentFrame(),parametersModel.getBiochamModel().getParameters(),null,null,null);	
		Object[] newParam=new Object[2];
		newParam=param.getNewParameter();
		String name=(String)newParam[0];
		String value=(String)newParam[1];		   				
		if(name!=null && value!=null){
		//	parametersModel.addParameter(name, value);
		//	parametersView.refresh();	
			StringBuilder sb=new StringBuilder();
			sb.append("parameter(");
			sb.append(name);
			sb.append(",");
			sb.append(value);
			sb.append(").\n");
			parametersModel.getBiochamModel().sendToBiocham(sb.toString());
			sb=null;
		
		}
		param=null;
		newParam=null; 
		name=null;
		value=null;
	}
	
	public void delete(String s){
		parametersModel.deleteParameter(s);
		parametersView.refresh();	
		StringBuilder sb=new StringBuilder();
		sb.append("delete_parameter(");
		sb.append(s);		
		sb.append(").\n");
		parametersModel.getBiochamModel().sendToBiocham(sb.toString());
		sb=null;
		
	}
	
	public ParametersModel getParametersModel() {
		return parametersModel;
	}

	public void setParametersModel(ParametersModel parametersModel) {
		this.parametersModel = parametersModel;
	}

	public ParametersView getParametersView() {
		return parametersView;
	}

	public void setParametersView(ParametersView parametersView) {
		this.parametersView = parametersView;
	}

	public ParametersListener getMouseListener() {
		return mouseListener;
	}

	public void setMouseListener(ParametersListener mouseListener) {
		this.mouseListener = mouseListener;
	}

	public ParameterKeyListener getKeyListener() {
		return keyListener;
	}

	public void setKeyListener(ParameterKeyListener keyListener) {
		this.keyListener = keyListener;
	}

	/**
	 * Listener that is responsible for initial state manipulation methods initiated from the InitialStateView.
	 * */
	class ParameterKeyListener extends KeyAdapter{
		
		public void keyPressed(KeyEvent e) {
			if(e.getKeyCode()==e.VK_ENTER){
				if(e.getSource() instanceof JFormattedTextField){
					if(((JFormattedTextField) e.getSource()).isEditable()){
							JFormattedTextField tf=(JFormattedTextField) e.getSource();							
							StringBuilder sb=new StringBuilder();
							sb.append("delete_parameter(");
							sb.append(tf.getName());		
							sb.append(").\n");
							parametersModel.getBiochamModel().sendToBiocham(sb.toString());							
							parametersModel.deleteParameter(tf.getName());
							parametersModel.addParameter(tf.getName(),tf.getText().trim());
							parametersView.refresh();
							sb.delete(0,sb.length());
							sb.append("parameter(");
							sb.append(tf.getName());
							sb.append(",");
							sb.append(tf.getText());
							sb.append(").\n");
							parametersModel.getBiochamModel().sendToBiocham(sb.toString());
							tf.setEditable(false);
							sb=null;
							
					}
				}
			}
			
		}
	}

	/**
	 * Listener that is responsible for parameters manipulation methods initiated from the InitialStateView.
	 * */
	class ParametersListener extends MouseAdapter{
		public void mouseClicked(MouseEvent e) {
			if(e.getSource() instanceof ModifyButton){
				Component[] cs=parametersView.getCentralPanel().getComponents();
				for(int i=0;i<cs.length;i++){
					Utils.debugMsg(cs[i].getClass().toString());
					if(cs[i] instanceof JFormattedTextField && cs[i].getName().equals(((ModifyButton)e.getSource()).getName())){
						((JFormattedTextField)cs[i]).setEditable(true);
						break;
					}
				}
			}		
		}
	}

	public DeleteParameter getDeleteListener() {
		return deleteListener;
	}

	public void setDeleteListener(DeleteParameter deleteListener) {
		this.deleteListener = deleteListener;
	}
	
	/**
	 * Listener that is responsible for deleting a particular parameter
	 * */
	class DeleteParameter extends MouseAdapter{
		public void mouseClicked(MouseEvent e) {
			Component button=(Component) e.getSource();
			String name=button.getName();
			delete(name);
			button=null;
			name=null;
			
			
		}
	}	
}
