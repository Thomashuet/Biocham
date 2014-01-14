package fr.inria.contraintes.biocham.dialogs;

import fr.inria.contraintes.biocham.BiochamMainFrame;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;



public class DialogChooseOptions extends JDialog {
	
	
	private static final long serialVersionUID = 1L;
	private String options;
	private JCheckBox op1,op2,op3,op4,op5;
    String result = null;
    private final Font commonFont = new Font("",Font.ITALIC,12);
	
	
	public DialogChooseOptions(JFrame parent){			
		super (parent, true);	  
	    pack();		
	}
	
	
	
	
	
	public void createDotOptionsDialog(){
		
		
		boolean b=SwingUtilities.isEventDispatchThread();
		
		setTitle("Dot Image Options");
		
		
	    
	    JPanel subject=new JPanel();
	    JLabel intro=new JLabel("Specify options for the Dot Image:");
	    intro.setHorizontalAlignment(SwingConstants.CENTER);
	    subject.add(new JLabel());
	    subject.add(new JLabel());
	    subject.add(intro);
	    subject.add(new JLabel());
	    	
	    
	    
	    
	    JPanel checkPanel = new JPanel(new GridLayout(0, 1));
        checkPanel.setBorder(BorderFactory.createEmptyBorder(5,5,5,5));
				
		JLabel l1=new JLabel("			    	To force the molecules present in the initial state to be shown on the top of the image.");
		l1.setFont(commonFont);		
		JLabel l2=new JLabel("			    	To show enzymes catalyzing a reaction or modifiers of some kind (i.e. both input and tarea)");
		JLabel l6=new JLabel("			    	with a double arrow (input/tarea) instead of a simple dashed arrow.");
		l2.setFont(commonFont);
		l6.setFont(commonFont);			
		JLabel l3=new JLabel("			    	To color in red the latest pathway returned by a CTL query.");
		l3.setFont(commonFont);		
		JLabel l4=new JLabel("			    	To produce A3 sized graphs instead of the default A4.");
		l4.setFont(commonFont);		
		JLabel l5=new JLabel("			    	To produce the state graph instead of the reaction graph.");
		l5.setFont(commonFont);
               
        op1 = new JCheckBox("init_up");
		op1.setName("init_up");
		op1.setHorizontalAlignment(SwingConstants.LEADING);
		op2 = new JCheckBox("mod_double");
		op2.setName("mod_double");
		op3 = new JCheckBox("col_path");
		op3.setName("col_path");
		op4 = new JCheckBox("double_size");
		op4.setName("double_size");
		op5 = new JCheckBox("state");
		op5.setName("state");
		
        checkPanel.add(op1);     
        checkPanel.add(new JLabel());
        checkPanel.add(l1);
        checkPanel.add(new JLabel());
        checkPanel.add(new JLabel());
        checkPanel.add(new JLabel());
        
        checkPanel.add(op2); 
        checkPanel.add(new JLabel());
        checkPanel.add(l2);
        checkPanel.add(l6);       
        checkPanel.add(new JLabel());
        checkPanel.add(new JLabel());
        checkPanel.add(new JLabel());
        
        checkPanel.add(op3);
        checkPanel.add(new JLabel());      
        checkPanel.add(l3);
        checkPanel.add(new JLabel());
        checkPanel.add(new JLabel());
        checkPanel.add(new JLabel());
        
        checkPanel.add(op4); 
        checkPanel.add(new JLabel());
        checkPanel.add(l4);
        checkPanel.add(new JLabel());
        checkPanel.add(new JLabel());
        checkPanel.add(new JLabel());
        
        checkPanel.add(op5); 
        checkPanel.add(new JLabel());
        checkPanel.add(l5);
        checkPanel.add(new JLabel());
        checkPanel.add(new JLabel());
        checkPanel.add(new JLabel());
                
        add(subject, BorderLayout.NORTH);
        add(checkPanel, BorderLayout.CENTER);
        
        
        
        
        JPanel buttonsPanel = new JPanel(); 
        
        JButton okButton = new JButton();
	    okButton.setText("OK");
	    okButton.setActionCommand("OK");
	    okButton.addActionListener(new ActionListener() {public void actionPerformed(ActionEvent event) { windowAction(event);}});
	    buttonsPanel.add(okButton);	      
	    
	    JButton cancelButton = new JButton();
	    cancelButton.setText("Cancel");
	    cancelButton.setActionCommand("CANCEL");
	    cancelButton.addActionListener(new ActionListener() { public void actionPerformed(ActionEvent event) { windowAction(event);}});
	    buttonsPanel.add(cancelButton);
	  
	    add(buttonsPanel,BorderLayout.SOUTH);
	    
	    
	    
	    
	    JFrame frame=BiochamMainFrame.frame;
	    Point pos = frame.getLocationOnScreen();
	    setLocation(pos.x+frame.getSize().width/2-185,pos.y+frame.getSize().height/2-140);
	    setResizable(false);        
	    setSize(new Dimension(800, 500));
	    setLocationRelativeTo(frame);
	    setVisible(true);	  
        
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
	      } else if (cmd.equals("CANCEL")) {
	    	  
	    	  closeWindow = true;
	    	  
	      } else if (cmd.equals("OK")) {	   
	    	  
	    	  String list_of_options="[";
	    	  boolean has=false;
	    	  try {
	    		  
	    		  if(op1.isSelected()){
	    			  
	    			  list_of_options+=op1.getName();
	    			  has=true;
	    		  }
	    		  if(op2.isSelected()){
	    			  if(has){list_of_options+=",";}
	    			  list_of_options+=op2.getName();
	    			  has=true;
	    		  }
	    		  if(op3.isSelected()){
	    			  if(has){list_of_options+=",";}
	    			  list_of_options+=op3.getName();	
	    			  has=true;
	    		  }
	    		  if(op4.isSelected()){
	    			  if(has){list_of_options+=",";}
	    			  list_of_options+=op4.getName();
	    			  has=true;
	    		  }
	    		  if(op5.isSelected()){
	    			  if(has){list_of_options+=",";}
	    			  list_of_options+=op5.getName();
	    			  has=true;
	    		  }
	    		  list_of_options+="]";
	    		  setOptions(list_of_options);
	    		  closeWindow = true;
	            
	    	  }catch  (Exception excp) {
	    		  excp.printStackTrace();
	    	  }
	      
	      } else {
	      }
	      
	      
	      
	      
	      if (closeWindow) {
	         setVisible(false);
	         dispose();
	      }
	      
	      
	      
	   }

	public String getOptions() {
		return options;
	}

	public void setOptions(String options) {
		this.options = options;
	}
	
}