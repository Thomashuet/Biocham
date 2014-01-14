package fr.inria.contraintes.biocham.modelData;
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
 * The Controller executes the corresponding methods on DeclarationView clicking events.
 * It updates the Declarations model and the DeclarationView view, and sends commands to Biocham.
 * 
 * */
public class DeclarationsController implements ActionListener{

	
	DeclarationsModel declarationsModel;
	DeclarationsView declarationsView;
	DeclarationsListener mouseListener;
	DeleteDeclaration deleteListener;	
	DeclarationKeyListener keyListener;
	
	
	
	/**
	 * Constructor saves references of the view and the model, and creates instances of declarations view's event listeners.
	 * */
	public DeclarationsController(DeclarationsModel model, DeclarationsView view){
		declarationsModel=model;
		declarationsView=view;				
		mouseListener=new DeclarationsListener();
		keyListener=new DeclarationKeyListener();
		deleteListener=new DeleteDeclaration();
	}

	public void actionPerformed(ActionEvent e) {
		if(e.getActionCommand().equals("addDeclaration")){
			add();
		}
	}
	
	public void add() {
		
		DialogAddParameter param=new DialogAddParameter(declarationsView.getParentFrame(),declarationsModel.getBiochamModel().getDeclarations(),null,null,null);	
		Object[] newParam=new Object[2];
		newParam=param.getNewParameter();
		String name=(String)newParam[0];
		String value=(String)newParam[1];		   				
		if(name!=null && value!=null){
			
			//declarationsModel.addDeclaration(name, value);
			//declarationsView.refresh();	
			StringBuilder sb=new StringBuilder();
			sb.append("declare ");//	String s="declare "+name+"~parts_of({"+value+"}).\n";  declare a~parts_of({p1,p2}).
			sb.append(name);
			sb.append("~parts_of({");
			sb.append(value);
			sb.append("}).\n");
			declarationsModel.getBiochamModel().sendToBiocham(sb.toString());
			sb=null;
			
		}
		param=null;
		newParam=null; 
		name=null;
		value=null;
	}
	
	public void delete(String s){
		StringBuilder sb=new StringBuilder();
		sb.append("delete_declaration(");
		sb.append(s);		
		sb.append(").\n");
		declarationsModel.getBiochamModel().sendToBiocham(sb.toString());
		sb=null;
		declarationsModel.deleteDeclaration(s);
		declarationsView.refresh();	
	}
	
	public DeclarationsModel getDeclarationsModel() {
		return declarationsModel;
	}

	public void setDeclarationsModell(DeclarationsModel declarationsModel) {
		this.declarationsModel = declarationsModel;
	}

	public DeclarationsView getDeclarationsView() {
		return declarationsView;
	}

	public void setParametersView(DeclarationsView declarationsView) {
		this.declarationsView = declarationsView;
	}

	public DeclarationsListener getMouseListener() {
		return mouseListener;
	}

	public void setMouseListener(DeclarationsListener mouseListener) {
		this.mouseListener = mouseListener;
	}
	
	/**
	 * Listener that is responsible for declarations manipulation methods initiated from the DeclarationsView.
	 * */
	class DeclarationsListener extends MouseAdapter{
		public void mouseClicked(MouseEvent e) {
			if(e.getSource() instanceof ModifyButton){
				Component[] cs=declarationsView.getCentralPanel().getComponents();
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

	public DeleteDeclaration getDeleteListener() {
		return deleteListener;
	}

	public void setDeleteListener(DeleteDeclaration deleteListener) {
		this.deleteListener = deleteListener;
	}
	
	public DeclarationKeyListener getKeyListener() {
		return keyListener;
	}

	public void setKeyListener(DeclarationKeyListener keyListener) {
		this.keyListener = keyListener;
	}

	/**
	 * Listener that is responsible for initial state manipulation methods initiated from the InitialStateView.
	 * */
	class DeclarationKeyListener extends KeyAdapter{
		
		public void keyPressed(KeyEvent e) {
			if(e.getKeyCode()==e.VK_ENTER){
				if(e.getSource() instanceof JFormattedTextField){
					if(((JFormattedTextField) e.getSource()).isEditable()){
							JFormattedTextField tf=(JFormattedTextField) e.getSource();
							StringBuilder sb=new StringBuilder();
							sb.append("delete_declaration(");
							sb.append(tf.getName());		
							sb.append(").\n");
							sb.append("declare ");//	String s="declare "+name+"~parts_of({"+value+"}).\n";  declare a~parts_of({p1,p2}).
							sb.append(tf.getName());
							sb.append("~parts_of({");
							sb.append(tf.getText());
							sb.append("}).\n");	
							tf.setEditable(false);
							declarationsModel.deleteDeclaration(tf.getName());
							declarationsView.refresh();
							declarationsModel.getBiochamModel().sendToBiocham(sb.toString());							
							
							/*declarationsModel.getDeclarations().put(tf.getName(),tf.getText().trim());
							declarationsView.refresh();	
							sb.delete(0,sb.length());*/													
							
							sb=null;
						
					}
				}
			}
			
		}
	}
	/**
	 * Listener that is responsible for deleting a particular declarations
	 * */
	class DeleteDeclaration extends MouseAdapter{
		public void mouseClicked(MouseEvent e) {
			Component button=(Component) e.getSource();
			String name=button.getName();
			delete(name); 
			button=null;
			name=null;
			
			
		}
	}	
	
}

