package fr.inria.contraintes.biocham.dialogs;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.customComponents.GradientPanel;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;

import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTextField;
import javax.swing.Spring;
import javax.swing.SpringLayout;

public class BiochamSmallDialog extends JDialog{

	
	Spring maxSpring=Spring.constant(30);
	Container contents;
	GradientPanel coverPanel;
	SpringLayout layout;
	JLabel textLabel;
	JTextField field;
	String mapKeyVal;
	String inputVal;
	boolean optionalVal;
	
	public BiochamSmallDialog(String title, String text, boolean optional, String optionalValue, String mapKey){
		 super (BiochamMainFrame.frame, true);
		 setTitle(title);
		 contents = getContentPane();
		 contents.setLayout(new BorderLayout());
		 coverPanel=new GradientPanel();
		 layout= new SpringLayout();
		 coverPanel.setLayout(layout);
		 contents.add(coverPanel,BorderLayout.CENTER);
		 
		 applyDifference();
		 
		 JFrame frame=BiochamMainFrame.frame;
   	  	 Point pos = frame.getLocationOnScreen();
   	  	 //setLocation(pos.x+frame.getSize().width/2-185,pos.y+frame.getSize().height/2-140);
   	  	 setLocation(pos.x+frame.getSize().width/2,pos.y+frame.getSize().height/2);
   	  	 setResizable(false);
   	  	 setSize(new Dimension(400, 200));	    
   	  	 setLocationRelativeTo(frame);
   	  	 setVisible(true);	 
	     pack();
	}
	
	public void applyDifference(){}
	
	public String getInputVal() {
		return inputVal;
	}
	public void setInputVal(String inputVal) {
		this.inputVal = inputVal;
	}
	
	public void windowAction(Object actionCommand) {
	      boolean closeWindow = false;
	      String cmd = null;
	      if (actionCommand != null) {
	         if (actionCommand instanceof ActionEvent) {
	            cmd = ((ActionEvent)actionCommand).getActionCommand();
	         } else {
	            cmd = actionCommand.toString();
	         }
	      }
	      if (cmd == null) {
	         // do nothing
	      }else if(cmd.equals("Ok")){
	    	
	    	  if(!optionalVal && (field.getText()==null || field.getText()=="")){
	    		  JOptionPane.showMessageDialog(BiochamMainFrame.frame,"You have to enter a value.");
	    	  }else{
	    		  inputVal=field.getText();
		    	  if(inputVal!=null && inputVal!=""){
		    		  BiochamDynamicTree.currentModel.simulationMap.put(mapKeyVal,inputVal);  
		    	  }	 
		    	  closeWindow=true;
	    	  }  	    	  
	      }else{
	    	  setVisible(false);
		      dispose();
	      }     
	      
	      if (closeWindow) {
	         setVisible(false);
	         dispose();
	      }
	  }
}
