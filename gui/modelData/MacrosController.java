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
 * The Controller executes the corresponding methods on MacrosView clicking events.
 * It updates the Macros model and the Macros view, and sends commands to Biocham.
 * 
 * */
public class MacrosController implements ActionListener{

	MacrosModel macrosModel;
	MacrosView macrosView;
	MacrosListener mouseListener;
	DeleteMacro deleteListener;	
	MacroKeyListener keyListener;
	/**
	 * Constructor saves references of the view and the model, and creates instances of rules view's event listeners.
	 * */
	public MacrosController(MacrosModel model, MacrosView view){
		macrosModel=model;
		macrosView=view;				
		mouseListener=new MacrosListener();
		keyListener=new MacroKeyListener();
		deleteListener=new DeleteMacro();
	}

	public void actionPerformed(ActionEvent e) {
		if(e.getActionCommand().equals("addMacro")){
			add();
		}
	}

	public void add() {
		
		DialogAddParameter param=new DialogAddParameter(macrosView.getParentFrame(),macrosModel.getBiochamModel().getMacros(),null,null,null);	
		Object[] newParam=new Object[2];
		newParam=param.getNewParameter();
		String name=(String)newParam[0];
		String value=(String)newParam[1];		   				
		if(name!=null && value!=null){
			//macrosModel.addMacro(name, value);
			//macrosView.refresh();	
			StringBuilder sb=new StringBuilder();
			sb.append("macro(");
			sb.append(name);
			sb.append(",");
			sb.append(value);
			sb.append(").\n");
			macrosModel.getBiochamModel().sendToBiocham(sb.toString());
			sb=null;
			
		}
		param=null;
		newParam=null; 
		name=null;
		value=null;
	}

	public void delete(String s){
		macrosModel.deleteMacro(s);
		macrosView.refresh();	
		StringBuilder sb=new StringBuilder();
		sb.append("delete_macro(");
		sb.append(s);		
		sb.append(").\n");
		macrosModel.getBiochamModel().sendToBiocham(sb.toString());
		sb=null;
		
	}
	
	public MacrosModel getMacrosModel() {
		return macrosModel;
	}

	public void setMacrosModel(MacrosModel macrosModel) {
		this.macrosModel = macrosModel;
	}

	public MacrosView getMacrosView() {
		return macrosView;
	}

	public void setMacrosView(MacrosView macrosView) {
		this.macrosView = macrosView;
	}

	public MacrosListener getMouseListener() {
		return mouseListener;
	}

	public void setMouseListener(MacrosListener mouseListener) {
		this.mouseListener = mouseListener;
	}
	
	/**
	 * Listener that is responsible for Macros manipulation methods initiated from the MacrosView.
	 * */
	class MacrosListener extends MouseAdapter{
		public void mouseClicked(MouseEvent e) {
			if(e.getSource() instanceof ModifyButton){
				Component[] cs=macrosView.getCentralPanel().getComponents();
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

	public DeleteMacro getDeleteListener() {
		return deleteListener;
	}

	public void setDeleteListener(DeleteMacro deleteListener) {
		this.deleteListener = deleteListener;
	}
	
	public MacroKeyListener getKeyListener() {
		return keyListener;
	}

	public void setKeyListener(MacroKeyListener keyListener) {
		this.keyListener = keyListener;
	}

	/**
	 * Listener that is responsible for initial state manipulation methods initiated from the InitialStateView.
	 * */
	class MacroKeyListener extends KeyAdapter{
		
		public void keyPressed(KeyEvent e) {
			if(e.getKeyCode()==e.VK_ENTER){
				if(e.getSource() instanceof JFormattedTextField){
					if(((JFormattedTextField) e.getSource()).isEditable()){
							JFormattedTextField tf=(JFormattedTextField) e.getSource();
							StringBuilder sb=new StringBuilder();
							sb.append("delete_macro(");
							sb.append(tf.getName());		
							sb.append(").\n");
							macrosModel.getBiochamModel().sendToBiocham(sb.toString());							
							macrosModel.deleteMacro(tf.getName());
							sb.delete(0,sb.length());							
							macrosModel.addMacro(tf.getName(),tf.getText().trim());
							macrosView.refresh();
							sb.append("macro(");
							sb.append(tf.getName());
							sb.append(",");
							sb.append(tf.getText());
							sb.append(").\n");
							macrosModel.getBiochamModel().sendToBiocham(sb.toString());
							tf.setEditable(false);
							sb=null;
							
					}
				}
			}
			
		}
	}
	
	/**
	 * Listener that is responsible for deleting a particular macro
	 * */
	class DeleteMacro extends MouseAdapter{
		public void mouseClicked(MouseEvent e) {
			Component button=(Component) e.getSource();
			String name=button.getName();
			delete(name);
			button=null;
			name=null;
			
			
		}
	}	
}

