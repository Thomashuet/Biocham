package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.WorkbenchToolBars;
import fr.inria.contraintes.biocham.customComponents.ModifyButton;
import fr.inria.contraintes.biocham.dialogs.DialogAddParameter;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Iterator;
import java.util.Map;

import javax.swing.JButton;
import javax.swing.JFormattedTextField;


/**
 * The Controller executes the corresponding methods on InitialStateView clicking events.
 * It updates the InitialState model and the InitialState view, and sends commands to Biocham.
 * 
 * */
public class InitialStateController implements ActionListener{

	InitialStateModel initStateModel;
	InitialStateView initStateView;
	InitStateListener mouseListener;
	InitStateKeyListener keyListener;
		
	/**
	 * Constructor saves references of the view and the model, and creates instances of rules view's event listeners.
	 * */
	public InitialStateController(InitialStateModel model, InitialStateView view){
		initStateModel=model;
		initStateView=view;				
		mouseListener=new InitStateListener();
		keyListener=new InitStateKeyListener();
	}

	public void actionPerformed(ActionEvent e) {
		if(e.getActionCommand().equals("addInitConc")){
			add();
		}else if(e.getActionCommand().equals("absentNotPresent")){
			setAbsentNotPresent();
		}else if(e.getActionCommand().equals("presentNotAbsent")){
			setPresentNotAbsent();
		}else if(e.getActionCommand().equals("allAbsent")){
			setAllAbsent();
		}else if(e.getActionCommand().equals("allPresent")){
			setAllPresent();
		}else if(e.getActionCommand().equals("allUndef")){
			setAllUndefined();
		}else if(e.getActionCommand().equals("Absent")){
			if(e.getSource() instanceof JButton){
				setAbsent(((JButton)e.getSource()).getName());	
			}			
		}else if(e.getActionCommand().equals("Present")){
			if(e.getSource() instanceof JButton){
				setPresent(((JButton)e.getSource()).getName());	
			}			
		}else if(e.getActionCommand().equals("Undefined")){
			if(e.getSource() instanceof JButton){
				setUndefined(((JButton)e.getSource()).getName());	
			}			
		}	
	}

	
	public InitialStateModel getInitStateModel() {
		return initStateModel;
	}

	public void setInitStateModel(InitialStateModel initStateModel) {
		this.initStateModel = initStateModel;
	}

	public InitialStateView getInitStateView() {
		return initStateView;
	}

	public void setInitStateView(InitialStateView initStateView) {
		this.initStateView = initStateView;
	}

	public InitStateListener getMouseListener() {
		return mouseListener;
	}

	public void setMouseListener(InitStateListener mouseListener) {
		this.mouseListener = mouseListener;
	}
	
	public InitStateKeyListener getKeyListener() {
		return keyListener;
	}

	public void setKeyListener(InitStateKeyListener keyListener) {
		this.keyListener = keyListener;
	}

	/**
	 * Listener that is responsible for initial state manipulation methods initiated from the InitialStateView.
	 * */
	class InitStateListener extends MouseAdapter{
		
		public void mouseClicked(MouseEvent e) {
			if(e.getSource() instanceof ModifyButton){
				Component[] cs=initStateView.getCentralPanel().getComponents();
				for(int i=0;i<cs.length;i++){
					if(cs[i] instanceof JFormattedTextField && cs[i].getName().equals(((ModifyButton)e.getSource()).getName())){
						((JFormattedTextField)cs[i]).setEditable(true);
						break;
					}
				}
			} 
		}
		public void mousePressed(MouseEvent e) {
			if(e.getSource() instanceof JFormattedTextField){
				if (e.getClickCount() == 2) {					
					e.getComponent().setBackground(Color.white);
					reset(e.getComponent().getName());			
				}					
			}else if(e.getSource() instanceof ModifyButton){
				Component[] cs=initStateView.getCentralPanel().getComponents();
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
	
	
	/**
	 * Listener that is responsible for initial state manipulation methods initiated from the InitialStateView.
	 * */
	class InitStateKeyListener extends KeyAdapter{
		
		public void keyPressed(KeyEvent e) {
			if(e.getKeyCode()==e.VK_ENTER){
				if(e.getSource() instanceof JFormattedTextField){
					if(((JFormattedTextField) e.getSource()).isEditable()){
							JFormattedTextField tf=(JFormattedTextField) e.getSource();
							String s=tf.getText();			    		
							tf.setValue(s);	
							
							double val=0;						
							try{
								val=Double.parseDouble(s);
								s=null;
							}catch(Exception ee){
								 /*System.out.println("The value not of numerical type "+tf.getText());
								 WorkbenchToolBars.infoLabel.setText("The value is not of numerical type"+tf.getText());
								 WorkbenchToolBars.infoLabel.setBalloonToolTipVisible(true);*/
							}
							//
							initStateModel.deleteInitState(tf.getName());
							initStateView.refresh();
							StringBuilder sb=new StringBuilder();
							sb.append("present(");
							sb.append(tf.getName());
							sb.append(",");
							sb.append(val);
							sb.append(").\n");
							initStateModel.getBiochamModel().sendToBiocham(sb.toString());
							
							tf.setEditable(false);
						
							sb=null;
							//initStateModel.getInitStates().put(tf.getName(),tf.getText().trim());
						
					}
				}
			}
			
		}
	}

	public void setPresent(String name){
		if(initStateModel.getInitStates().containsKey(name)){
			initStateModel.getInitStates().put(name, "1");
			StringBuilder sb=new StringBuilder();
			sb.append("present(");
			sb.append(name);
			sb.append(").\n");
			initStateModel.getBiochamModel().sendToBiocham(sb.toString());
			sb=null;
			//initStateView.refresh();
		}
	}
	public void setAbsent(String name){
		if(initStateModel.getInitStates().containsKey(name)){
			initStateModel.getInitStates().put(name, "0");
			StringBuilder sb=new StringBuilder();
			sb.append("absent(");
			sb.append(name);
			sb.append(").\n");
			initStateModel.getBiochamModel().sendToBiocham(sb.toString());
			sb=null;
			//initStateView.refresh();
		}
	}
	public void setUndefined(String name){
		if(initStateModel.getInitStates().containsKey(name)){
			initStateModel.getNotDefined().put(name, initStateModel.getInitStates().get(name));
			initStateModel.getInitStates().remove(name);
			StringBuilder sb=new StringBuilder();
			sb.append("undefined(");
			sb.append(name);
			sb.append(").\n");
			initStateModel.getBiochamModel().sendToBiocham(sb.toString());
			sb=null;
			initStateView.refresh();
		}
	}
	public void setAllPresent(){
		StringBuilder sb=new StringBuilder();
		sb.append("present({");
		int counter=0;
		for (Iterator it = initStateModel.getInitStates().entrySet().iterator(); it.hasNext();) {
		     Map.Entry entry = (Map.Entry)it.next();
		     if(Double.parseDouble(entry.getValue().toString())==0){
		    	 if(counter>0){
		    		 sb.append(",");
		    	 }
		    	 sb.append(entry.getKey().toString());
		    	 counter++;
		    	 entry.setValue(1);
		     }		     
		}
		sb.append("}).\n");		
		initStateModel.getBiochamModel().sendToBiocham(sb.toString());
		sb=null;
		initStateView.refresh();
	}
	public void setAllAbsent(){
		StringBuilder sb=new StringBuilder();
		sb.append("absent({");
		int counter=0;
		for (Iterator it = initStateModel.getInitStates().entrySet().iterator(); it.hasNext();) {
		     Map.Entry entry = (Map.Entry)it.next();
		     if(Double.parseDouble(entry.getValue().toString())>0){
		    	 if(counter>0){
		    		 sb.append(",");
		    	 }
		    	 sb.append(entry.getKey().toString());
		    	 counter++;
		    	 entry.setValue(0);
		     }		     
		}
		sb.append("}).\n");		
		initStateModel.getBiochamModel().sendToBiocham(sb.toString());
		sb=null;
		initStateView.refresh();
	}
	public void setAllUndefined(){
		StringBuilder sb=new StringBuilder();
		sb.append("undefined({");
		int counter=0;
		for (Iterator it = initStateModel.getInitStates().entrySet().iterator(); it.hasNext();) {
		     Map.Entry entry = (Map.Entry)it.next();
		     if(counter>0){
	    		 sb.append(",");
	    	 }
	    	 sb.append(entry.getKey().toString());
	    	 counter++;	    	 	     
		}
		sb.append("}).\n");		
		initStateModel.getBiochamModel().sendToBiocham(sb.toString());
		initStateModel.getNotDefined().putAll(initStateModel.getInitStates());
		initStateModel.deleteAll();
		sb=null;
		initStateView.refresh();
	}	
	
	public void setAbsentNotPresent(){			
		for (Iterator it = initStateModel.getNotDefined().entrySet().iterator(); it.hasNext();) {
		     Map.Entry entry = (Map.Entry)it.next();		     
	    	 entry.setValue(0);	
	    	 Utils.debugMsg("Putting back: "+entry.getKey()+","+entry.getValue());
		}			
		Utils.debugMsg("Before size is: "+initStateModel.getInitStates().size());
		initStateModel.getInitStates().putAll(initStateModel.getNotDefined());		
		initStateModel.getNotDefined().clear();						
		Utils.debugMsg("After size is: "+initStateModel.getInitStates().size());
		initStateModel.getBiochamModel().sendToBiocham("make_absent_not_present.\n");		
		initStateView.refresh();
	}
	public void setPresentNotAbsent(){
		for (Iterator it = initStateModel.getNotDefined().entrySet().iterator(); it.hasNext();) {
		     Map.Entry entry = (Map.Entry)it.next();		     
	    	 entry.setValue(1);	     
		}				
		initStateModel.getInitStates().putAll(initStateModel.getNotDefined());
		initStateModel.getNotDefined().clear();						
		initStateModel.getBiochamModel().sendToBiocham("make_present_not_absent.\n");		
		initStateView.refresh();
	}
	
	public void reset(String name) {
		if(initStateModel.getStartInitialState().get(name)!=initStateModel.getInitStates().get(name)){
			StringBuilder sb=new StringBuilder();
			sb.append("present(");
			sb.append(name);
			sb.append(",");
			sb.append(initStateModel.getStartInitialState().get(name));
			sb.append(").\n");
			initStateModel.getBiochamModel().sendToBiocham(sb.toString());
			//initStateModel.getInitStates().put(name,initStateModel.getStartInitialState().get(name));
			sb=null;		
			//initStateView.refresh();
			
		}		
		
	}	
	public void add() {
		
		DialogAddParameter param=new DialogAddParameter(initStateView.getParentFrame(),initStateModel.getBiochamModel().getInitConditions(),null,null,null);	
		Object[] newParam=new Object[2];
		newParam=param.getNewParameter();
		String name=(String)newParam[0];
		String value=(String)newParam[1];		   				
		if(name!=null && value!=null){
		//	initStateModel.getInitStates().put(name, value);
			StringBuilder sb=new StringBuilder();
			sb.append("present(");
			sb.append(name);
			sb.append(",");
			sb.append(value);
			sb.append(").\n list_molecules.\n");
			initStateModel.getBiochamModel().sendToBiocham(sb.toString());
			sb=null;
	//		initStateModel.addInitState(name, value);
		//	initStateView.refresh();	
		}
		param=null;
		newParam=null; 
		name=null;
		value=null;
	}
	
}
