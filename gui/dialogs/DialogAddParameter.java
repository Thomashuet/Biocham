package fr.inria.contraintes.biocham.dialogs;

import fr.inria.contraintes.biocham.BiochamModelElement;
import fr.inria.contraintes.biocham.customComponents.GradientPanel;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.Utils;
import net.java.balloontip.BalloonTip;
import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.Spring;
import javax.swing.SpringLayout;




public class DialogAddParameter extends JDialog {

	
	
	   private String CMD_CANCEL = "cmd.cancel";
	   private String CMD_OK = "cmd.ok";	   
	   private  String name,value="";	   
	   private BiochamModelElement modelElement;	   
	   private boolean existence=false; 
	   JTextField nameText,valueText,eventText, timeText;
	   JLabel nameLabel,valueLabel,eventLabel, exampleLabel, timeLabel;
	   String result = null;
	   JPanel panel;
	   boolean label=true;
	   boolean event=false;
	   BalloonTip validationTip;
	   JButton okButton;
	   JFrame parentFrame;
	   boolean timeEvent=false;
	   
	   public DialogAddParameter(JFrame parent,BiochamModelElement element,String n,String v,JPanel pan) {
		   
	      super (parent, true);	 
	      parentFrame=parent;
	      modelElement=element;
	      panel=pan;
	      initComponents(n,v);
	      pack();
	   }
	   
	   private void initComponents(String n,String v) {
	     
	      // initialize the window
	      setTitle("Add New Value");
	      Container contents = getContentPane();
	      GradientPanel p=new GradientPanel();
	      contents.setLayout(new BorderLayout());
	      contents.add(p,BorderLayout.CENTER);	      
	      SpringLayout layout = new SpringLayout();
	      p.setLayout(layout);
	      nameLabel = new JLabel("Name: ");
	      valueLabel = new JLabel("Value: ");
	      exampleLabel=new JLabel();
	      nameText=new JTextField(7);
	      valueText = new JTextField(7);
	      nameText.setText("EnterName");
	      String warning="Incorrect name!\n A name is a word of alphanumerical and ’_’ \ncharacters beginning with a letter.";
	      final BalloonTip bTip=new BalloonTip(nameText,"Incorrect name!",Utils.parameter,BalloonTip.Orientation.RIGHT_BELOW,BalloonTip.AttachLocation.SOUTHEAST,2,2,false);
	      bTip.setText("Incorrect name!");
	      bTip.setIcon(Icons.icons.get("warning_16.png"));
	      bTip.setIconTextGap(10);	
	      bTip.setVisible(false);
	      bTip.enableClickToHide(true);
	      validationTip=bTip;

	      String modul=modelElement.getElementName();
	     
	      
	      if(modul.contains("Parameters") || modul.contains("Initial")){
	    	 
	    	  nameText.setText("EnterName");
	    	  valueText.setText("     Float       ");
	      
	      }else if(modul.contains("Events")){
	    	  if(v!=null){
	    		  setTimeEvent(true);
	    		  timeText=new JTextField(" Expression ");	    		
	    		  timeLabel=new JLabel("Time: ");
	    	  }
	    	  putAddingExample(modul,isTimeEvent());
	    	  nameLabel.setText("Parameter: ");
	    	  nameText=new JTextField(" EnterParameterName  ");
	    	  eventLabel = new JLabel("Condition: ");
	    	  eventText = new JTextField("  Help Compose  ");
	    	  eventText.addMouseListener(new MouseAdapter(){
	    		  public void mouseClicked(MouseEvent e) {
	    				if(e.getClickCount()==2){
	    					DialogAddSpecification params=new DialogAddSpecification(parentFrame, modelElement,"Condition Operators",panel);					
	    					String value=params.getFormula();
	    					eventText.setText(value);	
	    				}	    				
	    			}
	    	  });
	    	  
	    	  valueLabel = new JLabel("Kinetics: ");	    	
	    	  valueText = new JTextField(" Help Compose ");
	    	  valueText.addMouseListener(new MouseAdapter(){
	    		  public void mouseClicked(MouseEvent e) {
	    				if(e.getClickCount()==2){
	    					DialogAddSpecification params=new DialogAddSpecification(parentFrame, modelElement,"Events Operators",panel);					
	    					String value=params.getFormula();
	    					valueText.setText(value);	
	    				}	    				
	    			}
	    	  });
	      }else if(modul.contains("Volumes")){
	    	  
	    	  valueText = new JTextField(" Help Compose  ");  
	    	  valueText.addMouseListener(new MouseAdapter(){
	    		  public void mouseClicked(MouseEvent e) {
	    				if(e.getClickCount()==2){
	    					DialogAddSpecification params=new DialogAddSpecification(parentFrame, modelElement,"Volumes Operators",panel);					
	    					String value=params.getFormula();
	    					valueText.setText(value);	
	    				}	    				
	    			}
	    	  });
	      }else if(modul.contains("Declarations")){
	    	  
	    	  valueText = new JTextField("Phosphorylation sites");
	    	  
	      }else if(modul.contains("Macro")){
	    	  
	    	  valueText = new JTextField(" Help Compose ");
	    	  valueText.addMouseListener(new MouseAdapter(){
	    		  public void mouseClicked(MouseEvent e) {
	    				if(e.getClickCount()==2){
	    					DialogAddSpecification params=new DialogAddSpecification(parentFrame, modelElement,"Macro Operators",panel);					
	    					String value=params.getFormula();
	    					valueText.setText(value);	
	    				}	    				
	    			}
	    	  });
	      
	      }else if(modul.contains("Boolean Temporal")){
	    	  
	      }else if(modul.contains("Numerical Temporal")){
	    	  
	      }	        	
	      
	      if(modul.contains("Events")){
	    	  if(v!=null){
	    		  p.add(timeText);	      
		    	  p.add(timeLabel);	    		
	    	  }
	    	  p.add(nameLabel);	      
	    	  p.add(nameText);
	    	  p.add(valueLabel);
	    	  p.add(valueText);
	    	  p.add(eventLabel);
		      p.add(eventText);
		      
	      }else{
	    	  p.add(nameLabel);	      
	    	  p.add(nameText);
	    	  p.add(valueLabel);
	    	  p.add(valueText);
	      }
	      	      
	      p.add(exampleLabel);
	      
	      if(!modul.contains("Events")){
	    	  
	    	  layout.putConstraint(SpringLayout.WEST, nameLabel, 10,SpringLayout.WEST, p);
	    	  layout.putConstraint(SpringLayout.NORTH, nameLabel, 30, SpringLayout.NORTH, p);
	    	  layout.putConstraint(SpringLayout.WEST, exampleLabel, 10,SpringLayout.WEST, p);
	    	  layout.putConstraint(SpringLayout.NORTH, exampleLabel, 15, SpringLayout.SOUTH, nameLabel);
	    	  layout.putConstraint(SpringLayout.WEST, nameText, 5,SpringLayout.EAST, nameLabel);
	    	  layout.putConstraint(SpringLayout.NORTH, nameText, 30,SpringLayout.NORTH, p);
	    	  layout.putConstraint(SpringLayout.NORTH, valueLabel, 30, SpringLayout.NORTH, p);	      
	    	  layout.putConstraint(SpringLayout.WEST, valueLabel, 15,SpringLayout.EAST, nameText);
	    	  layout.putConstraint(SpringLayout.WEST, valueText, 5,SpringLayout.EAST, valueLabel);
	    	  layout.putConstraint(SpringLayout.NORTH, valueText, 30, SpringLayout.NORTH, p);
	      }else{
	    	  int h=30;
	    	  if(v!=null){
	    		  p.add(timeText);	      
		    	  p.add(timeLabel);	
		    	  layout.putConstraint(SpringLayout.WEST, timeLabel, 10,SpringLayout.WEST, p);
		    	  layout.putConstraint(SpringLayout.NORTH, timeLabel, 10, SpringLayout.NORTH, p);
		    	  layout.putConstraint(SpringLayout.WEST, timeText, 5,SpringLayout.EAST, timeLabel);
		    	  layout.putConstraint(SpringLayout.NORTH, timeText, 7,SpringLayout.NORTH, p);
		    	  h=45;
	    	  }    	  
	    	  
	    	  layout.putConstraint(SpringLayout.WEST, eventLabel, 10,SpringLayout.WEST, p);
	    	  layout.putConstraint(SpringLayout.NORTH, eventLabel, h, SpringLayout.NORTH, p);
	    	  layout.putConstraint(SpringLayout.WEST, exampleLabel, 10,SpringLayout.WEST, p);
	    	  layout.putConstraint(SpringLayout.NORTH, exampleLabel, 15, SpringLayout.SOUTH, eventLabel);
	    	  layout.putConstraint(SpringLayout.WEST, eventText, 5,SpringLayout.EAST, eventLabel);
	    	  layout.putConstraint(SpringLayout.NORTH, eventText, h,SpringLayout.NORTH, p);
	    	  
	    	  layout.putConstraint(SpringLayout.NORTH, nameLabel, h, SpringLayout.NORTH, p);	      
	    	  layout.putConstraint(SpringLayout.WEST, nameLabel, 15,SpringLayout.EAST, eventText);
	    	  layout.putConstraint(SpringLayout.WEST, nameText, 5,SpringLayout.EAST, nameLabel);
	    	  layout.putConstraint(SpringLayout.NORTH, nameText, h, SpringLayout.NORTH, p);
	    	  
	    	  layout.putConstraint(SpringLayout.NORTH, valueLabel, h, SpringLayout.NORTH, p);	      
	    	  layout.putConstraint(SpringLayout.WEST, valueLabel, 15,SpringLayout.EAST, nameText);
	    	  layout.putConstraint(SpringLayout.WEST, valueText, 5,SpringLayout.EAST, valueLabel);
	    	  layout.putConstraint(SpringLayout.NORTH, valueText, h, SpringLayout.NORTH, p);
	    	  
	      }
	      
	    
	      // Buttons
	      okButton = new JButton();
	      okButton.setText("OK");
	      okButton.setActionCommand(CMD_OK);	      
	      okButton.addActionListener(new ActionListener() {public void actionPerformed(ActionEvent event) { windowAction(event);}});
	      p.add(okButton);
	      if(!modul.contains("Events")){
	    	  Spring con=layout.getConstraint(SpringLayout.WEST,nameText);
	    	  layout.putConstraint(SpringLayout.NORTH, okButton, 40,SpringLayout.SOUTH, valueText);
	    	  layout.putConstraint(SpringLayout.WEST, okButton, Spring.sum(con,Spring.constant(60)),SpringLayout.WEST, p);
	      }else{
	    	  Spring con=layout.getConstraint(SpringLayout.WEST,nameLabel);
	    	  layout.putConstraint(SpringLayout.NORTH, okButton, 50,SpringLayout.SOUTH, valueText);
	    	  layout.putConstraint(SpringLayout.WEST, okButton, Spring.sum(con,Spring.constant(50)),SpringLayout.WEST, p);
	      }

	      JButton cancelButton = new JButton();
	      cancelButton.setText("Cancel");
	      cancelButton.setActionCommand(CMD_CANCEL);
	      cancelButton.addActionListener(new ActionListener() { public void actionPerformed(ActionEvent event) { windowAction(event);}});
	      p.add(cancelButton);
	      if(modul.contains("Events")){
	    	  layout.putConstraint(SpringLayout.NORTH, cancelButton, 50, SpringLayout.SOUTH, valueText);
	    	  layout.putConstraint(SpringLayout.WEST, cancelButton, 10,SpringLayout.EAST, okButton);
	      }else{
	    	  layout.putConstraint(SpringLayout.NORTH, cancelButton, 40, SpringLayout.SOUTH, valueText);
	    	  layout.putConstraint(SpringLayout.WEST, cancelButton, 10,SpringLayout.EAST, okButton);
	      }
	      
	      JFrame frame=parentFrame;
	      Point pos = frame.getLocationOnScreen();
	      Utils.debugMsg(pos.x+","+pos.y);
	      Utils.debugMsg(frame.getSize().width/2+","+frame.getSize().height/2);
	      
	      setLocation(pos.x+frame.getSize().width/2,pos.y+frame.getSize().height/2);
	      setResizable(false); 
	      if(modul.contains("Events")){
	    	  setSize(new Dimension(680, 190));
	      }else {
	    	  setSize(new Dimension(380, 170));
	      }/*else{
	    	  setSize(new Dimension(350, 150));
	      }	 */    
	      setLocationRelativeTo(frame);
	      setVisible(true);	      

	   }

	   
	   public Object[] getNewParameter(){
		   
			
		   Object[] parameter=new Object[2];
		   parameter[0]=getName();
		   parameter[1]=getValue();
		   return parameter;
	   } 
	   
	   
	   private void putAddingExample(String modul, boolean timeEvent){
		   if(modul.contains("Parameter")){
			   exampleLabel.setText("<html><i>Example: name: k1 value: 24  </i></html>");
		   }else if(modul.contains("Initial")){
			   exampleLabel.setText("<html><i>Example: name: Cdc2 value: 2.587  </i></html>");
		   }else if(modul.contains("Event")){
			   if(timeEvent){
				   exampleLabel.setText("<html><i>Example: Time: 2.0 condition: [X]<=10 parameters: k1,k2 values: k8,45  </i></html>");	    		
			   }else{
				   exampleLabel.setText("<html><i>Example: condition: [X]<=2 parameters: k1,k2 values: k8,45  </i></html>");   
			   }
			   
		   }else if(modul.contains("Volume")){
			   exampleLabel.setText("<html><i>Example: name: cyto value: 2  </i></html>");
		   }else if(modul.contains("Declaration")){
			   exampleLabel.setText("<html><i>Example: name: Cdc2 value: p1,p2,p3  </i></html>");
		   }else if(modul.contains("Macro")){
			   exampleLabel.setText("<html><i>Example: name: YT value: 24*k1-k2*[Cdc2]  </i></html>");
		   }
		   
	   }
	  
	   
	   /**
	    * The user has selected an option.
	    * If actionCommand is an ActionEvent, getCommandString() is called,
	    * otherwise toString() is used to get the action command.
	    *
	    * @param actionCommand may be null
	    */
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
	      } else if (cmd.equals(CMD_CANCEL)) {
	    	  setName(null);	        	 
	          setValue(null);
	         closeWindow = true;
	      } else if (cmd.equals(CMD_OK)) {	    	  
	    	  try {	
	    		  if(modelElement.getElementName().contains("Events")){
	    			  if(isTimeEvent()){
	    				  name=timeText.getText()+";"+eventText.getText()+";"+nameText.getText();
	    			  }else{
	    				  name=eventText.getText()+";"+nameText.getText();  
	    			  }
	    			  
	    			  setName(name);
	    		  }else{
	    			  name=nameText.getText();
	    			  setName(name);	
	    		  }
	    		  value=valueText.getText();
	    		  setValue(value);	    	
	             closeWindow = true;
	            
	         } catch  (Exception excp) {
	        	 
	        	 JOptionPane.showMessageDialog(this,"Invalid value: "+value,"Invalid value",JOptionPane.WARNING_MESSAGE);
	         }
	         
	      } else {
	      }
	      
	      
	      if (closeWindow) {
	         setVisible(false);
	         dispose();
	      }
	      
	   }
	   	   

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	public boolean isExistence() {
		return existence;
	}
	
	public boolean getExistence(){
		return isExistence();
	}

	public void setExistence(boolean existence) {
		this.existence = existence;
	}

	public boolean isTimeEvent() {
		return timeEvent;
	}

	public void setTimeEvent(boolean timeEvent) {
		this.timeEvent = timeEvent;
	}
	
}
