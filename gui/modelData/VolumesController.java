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
 * The Controller executes the corresponding methods on VolumesView clicking events.
 * It updates the Volumes model and the VolumesView, and sends commands to Biocham.
 * 
 * */
public class VolumesController implements ActionListener{

	VolumesModel volumesModel;
	VolumesView volumesView;
	VolumesListener mouseListener;
	DeleteVolume deleteListener;	
	VolumeKeyListener keyListener;
	/**
	 * Constructor saves references of the view and the model, and creates instances of rules view's event listeners.
	 * */
	public VolumesController(VolumesModel model, VolumesView view){
		volumesModel=model;
		volumesView=view;				
		mouseListener=new VolumesListener();
		keyListener=new VolumeKeyListener();
		deleteListener=new DeleteVolume();
	}

	public void actionPerformed(ActionEvent e) {
		if(e.getActionCommand().equals("addVolume")){
			add();
		}
	}
	public void add() {
		
		DialogAddParameter param=new DialogAddParameter(volumesView.getParentFrame(),volumesModel.getBiochamModel().getVolumes(),null,null,null);	
		Object[] newParam=new Object[2];
		newParam=param.getNewParameter();
		String name=(String)newParam[0];
		String value=(String)newParam[1];		   				
		if(name!=null && value!=null){
		/*	volumesModel.addVolume(name, value);
			volumesView.refresh();	*/
			StringBuilder sb=new StringBuilder();
			sb.append("volume(");
			sb.append(name);
			sb.append(",");
			sb.append(value);
			sb.append(").\n");
			volumesModel.getBiochamModel().sendToBiocham(sb.toString());
			sb=null;
			
		}
		param=null;
		newParam=null; 
		name=null;
		value=null;
	}

	public void delete(String s){
		volumesModel.deleteVolume(s);
		volumesModel.addVolume(s, "1");		
		volumesView.refresh();	
		StringBuilder sb=new StringBuilder();
		sb.append("volume(");
		sb.append(s);
		sb.append(",");
		sb.append(1);
		sb.append(").\n");
		volumesModel.getBiochamModel().sendToBiocham(sb.toString());
		sb=null;		
	}
	
	public VolumesModel getVolumesModel() {
		return volumesModel;
	}

	public void setVolumesModel(VolumesModel volumesModel) {
		this.volumesModel = volumesModel;
	}

	public VolumesView getVolumesView() {
		return volumesView;
	}

	public void setVolumesView(VolumesView volumesView) {
		this.volumesView = volumesView;
	}

	public VolumesListener getMouseListener() {
		return mouseListener;
	}

	public void setMouseListener(VolumesListener mouseListener) {
		this.mouseListener = mouseListener;
	}
	
	/**
	 * Listener that is responsible for Volumes manipulation methods initiated from the VolumesView.
	 * */
	class VolumesListener extends MouseAdapter{
		public void mouseClicked(MouseEvent e) {
			if(e.getSource() instanceof ModifyButton){
				Component[] cs=volumesView.getCentralPanel().getComponents();
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

	public DeleteVolume getDeleteListener() {
		return deleteListener;
	}

	public void setDeleteListener(DeleteVolume deleteListener) {
		this.deleteListener = deleteListener;
	}
	
	public VolumeKeyListener getKeyListener() {
		return keyListener;
	}

	public void setKeyListener(VolumeKeyListener keyListener) {
		this.keyListener = keyListener;
	}

	/**
	 * Listener that is responsible for initial state manipulation methods initiated from the InitialStateView.
	 * */
	class VolumeKeyListener extends KeyAdapter{
		
		public void keyPressed(KeyEvent e) {
			if(e.getKeyCode()==e.VK_ENTER){
				if(e.getSource() instanceof JFormattedTextField){
					if(((JFormattedTextField) e.getSource()).isEditable()){
							JFormattedTextField tf=(JFormattedTextField) e.getSource();
							tf.setEditable(false);
							volumesView.bTip.setVisible(false);
							volumesView.bTip.setEnabled(false);
							volumesView.bTip.closeBalloon();
							volumesView.bTip=null;
							//volumesModel.deleteVolume(tf.getName());							
							StringBuilder sb=new StringBuilder();
							sb.append("volume(");
							sb.append(tf.getName());
							sb.append(",");
							sb.append(tf.getText());
							sb.append(").\n");
							volumesModel.getBiochamModel().sendToBiocham(sb.toString());
							//volumesModel.addVolume(tf.getName(),tf.getText().trim());
						
							volumesView.refresh();
							//tf.setEditable(false);
							sb=null;
							
					}
				}
			}
			
		}
	}
	
	/**
	 * Listener that is responsible for deleting a particular Volume
	 * */
	class DeleteVolume extends MouseAdapter{
		public void mouseClicked(MouseEvent e) {
			Component button=(Component) e.getSource();
			String name=button.getName();
			delete(name);  
			button=null;
			name=null;
			
			
		}
	}	
}


