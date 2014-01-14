package fr.inria.contraintes.biocham.dialogs;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.customComponents.GradientPanel;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTextField;
import javax.swing.Spring;
import javax.swing.SpringLayout;

public class CustomInputDialog extends JDialog{
	

	Spring maxSpring=Spring.constant(30);
	Container contents;
	GradientPanel coverPanel;
	SpringLayout layout;
	JLabel textLabel;
	JTextField field;
	String mapKeyVal;
	String inputVal;
	JComboBox cb;
	boolean optionalVal;
	
	public CustomInputDialog(JFrame parent, String title, String text, boolean optional, String optionalValue, String mapKey,String[] choices){
		 super (parent, true);
		
		 setTitle(title);
		 contents = getContentPane();
		 contents.setLayout(new BorderLayout());
		 coverPanel=new GradientPanel();
		 layout= new SpringLayout();
		 coverPanel.setLayout(layout);
		 contents.add(coverPanel,BorderLayout.CENTER);
		 mapKeyVal=mapKey;
		 optionalVal=optional;
		 
		 textLabel = new JLabel(text);
		 coverPanel.add(textLabel);
		 layout.putConstraint(SpringLayout.WEST, textLabel, 30,SpringLayout.WEST, coverPanel);
   	  	 layout.putConstraint(SpringLayout.NORTH, textLabel, 30, SpringLayout.NORTH, coverPanel);
   	  	 
		 if(choices!=null){
			 cb=new JComboBox(choices);
			 cb.setEditable(false);
			 cb.setOpaque(false);			
			 cb.setSelectedItem(0);
			 cb.setPreferredSize(new Dimension(500, 20));
			 coverPanel.add(cb);
			 layout.putConstraint(SpringLayout.WEST, cb, 30,SpringLayout.WEST, coverPanel);
	   	  	 layout.putConstraint(SpringLayout.NORTH, cb, 30,SpringLayout.SOUTH, textLabel);  
		 }else{
			 field = new JTextField(optionalValue);   	  	 
	   	  	 String txt=BiochamDynamicTree.currentModel.simulationMap.get(mapKey);
	   	  	 if(txt!=null){
	   	  		 field.setText(txt);
	   	  	 }else{
	   	  		 field.setText(optionalValue);
	   	  	 }	    	
	   	  	 field.setColumns(25);
	   	  	 coverPanel.add(field);
	   	  	 layout.putConstraint(SpringLayout.WEST, field, 30,SpringLayout.WEST, coverPanel);
	   	  	 layout.putConstraint(SpringLayout.NORTH, field, 10,SpringLayout.SOUTH, textLabel);  
		 }
		 		    
   	  	 		      
   	  				      
   	  
   	  	 	
   	  
   	  	 JButton okButton = new JButton();
   	  	 okButton.setText("Ok");
   	  	 okButton.setActionCommand("Ok");
   	  	 okButton.addActionListener(new ActionListener() {public void actionPerformed(ActionEvent event) { windowAction(event);}});
   	  	 coverPanel.add(okButton);   	  	 	
   	  	 JButton cancelButton = new JButton();
   	  	 cancelButton.setText("  Cancel  ");
   	  	 cancelButton.setActionCommand("Cancel");
   	  	 cancelButton.addActionListener(new ActionListener() { public void actionPerformed(ActionEvent event) { windowAction(event);}});
   	  	 coverPanel.add(cancelButton);
   	  	 if(cb!=null){
   	  		 layout.putConstraint(SpringLayout.NORTH, okButton, 120, SpringLayout.SOUTH, textLabel);
   	  		 layout.putConstraint(SpringLayout.NORTH, cancelButton, 120, SpringLayout.SOUTH, textLabel);
   	  	 }else{
   	  		 layout.putConstraint(SpringLayout.NORTH, okButton, 60, SpringLayout.SOUTH, textLabel);
	  		 layout.putConstraint(SpringLayout.NORTH, cancelButton, 60, SpringLayout.SOUTH, textLabel);
   	  	 }
   	  	 
   	  	 		     
   	  	 layout.putConstraint(SpringLayout.WEST, okButton, 180,SpringLayout.WEST, textLabel);
   	  	 layout.putConstraint(SpringLayout.WEST, cancelButton, 10,SpringLayout.EAST, okButton);
	     
   	  	 JFrame frame=BiochamMainFrame.frame;
   	  	 Point pos = frame.getLocationOnScreen();
   	  	 setLocation(pos.x+frame.getSize().width/2-185,pos.y+frame.getSize().height/2-140);
   	  	 setResizable(false);
   	  	 if(cb!=null){
   	  		 setSize(new Dimension(580, 320));	 
   	  	 }else{
   	  		 setSize(new Dimension(400, 200));
   	  	 }
   	  	 	    
   	  	 setLocationRelativeTo(frame);
   	  	 setVisible(true);	 
	     pack();
	}
	
	
	
	  private void windowAction(Object actionCommand) {
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
	    	
	    	  if(cb!=null){
	    		  inputVal=cb.getSelectedItem().toString();
	    		  closeWindow=true;
	    	  }else{
	    		  if(!optionalVal && (field.getText()==null || field.getText()=="")){
		    		  JOptionPane.showMessageDialog(BiochamMainFrame.frame,"You have to enter a value.");
		    	  }else{
		    		  inputVal=field.getText();
			    	  if(inputVal!=null && inputVal!=""){
			    		  BiochamDynamicTree.currentModel.simulationMap.put(mapKeyVal,inputVal);  
			    	  }	 
			    	  closeWindow=true;
		    	  }  	  
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



	public String getInputVal() {
		return inputVal;
	}
	public void setInputVal(String inputVal) {
		this.inputVal = inputVal;
	}
}
