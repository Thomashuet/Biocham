package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.BiochamModelElement;
import fr.inria.contraintes.biocham.WorkbenchArea;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.dialogs.DialogAddSpecification;
import fr.inria.contraintes.biocham.dialogs.DialogOptions;
import fr.inria.contraintes.biocham.dialogs.DialogSearchParams;
import fr.inria.contraintes.biocham.dialogs.SatisfactionDegreeDialog;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.SupportedSuffixes;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;
import net.java.balloontip.BalloonTip;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Vector;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.Spring;
import javax.swing.SpringLayout;



public class ParamTableLTLSpecifications implements Parameters, ActionListener{

	
	Vector<LTLSpecification> specifications;
	WorkbenchArea biocham;
	JPanel panel;
	BiochamModel model;
	BiochamModelElement element;
	int savedResponses;
	ArrayList<String> origValues;
	boolean notToAdd=false;
	int north=0;
	JPanel checkingPanel,commandsPanel;
	SpringLayout sMC;
	JTextArea tarea;
	JPanel buttonPanel;
	private boolean fromBeggining=true;
	private boolean check=false;
	BalloonTip bTip;
	boolean newAdded=false;
	boolean modifying=false;
	String lastAdded="";
	boolean stopPrinting=false;
	HashMap<String,String> foundParameters=new HashMap<String,String>();
	boolean found=false;
	boolean cmdFinished=false;
	
	LTLModel ltlModel;
	LTLView view;
	
	
	public LTLModel getLtlModel() {
		return ltlModel;
	}
	public void setLtlModel(LTLModel ltlModel) {
		this.ltlModel = ltlModel;
	}
	
	public LTLView getView() {
		return view;
	}
	public void setView(LTLView view) {
		this.view = view;
	}
	public int getPanelCurrentX(){
		int x=0;
		Spring maxSpring = Spring.constant(10);
		SpringLayout layout = (SpringLayout) panel.getLayout();
		int i=1;
		if(panel.getComponentCount()>3){
			i=3;
		}
		maxSpring=layout.getConstraint(SpringLayout.WEST, panel.getComponent(panel.getComponentCount()-i));
		x=maxSpring.getValue()+80;
		return x;
	}
	public int getPanelCurrentY(){
		int y=0;
		Spring maxSpring = Spring.constant(10);
		SpringLayout layout = (SpringLayout) panel.getLayout();
		maxSpring=layout.getConstraint(SpringLayout.NORTH, panel.getComponent(panel.getComponentCount()-1));
		y=maxSpring.getValue()+80;
		return y;
	}
	
	public boolean isFound() {
		return found;
	}


	public boolean isNewAdded() {
		return newAdded;
	}


	public void setNewAdded(boolean newAdded) {
		this.newAdded = newAdded;
	}


	public int getNorth() {
		return north;
	}


	public void setNorth(int north) {
		this.north = north;
	}

	

	public ParamTableLTLSpecifications(BiochamModel m,WorkbenchArea workbench, JPanel p){
		
		biocham=workbench;
		model=m;
		panel=p;
		specifications=new Vector<LTLSpecification>();
		savedResponses=-1;
		origValues=new ArrayList<String>();
		
		commandsPanel=new JPanel();			 
		commandsPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createLineBorder(Color.black),"Checking LTL(R) Properties"));
		commandsPanel.setBackground(Utils.backgroundColor);
		commandsPanel.setName("Checking LTL Properties");
		sMC=new SpringLayout();
		commandsPanel.setLayout(sMC);
		
		checkingPanel=new JPanel();			 
		checkingPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createLineBorder(Color.black),"Results"));
		checkingPanel.setLayout(new BorderLayout());	
		checkingPanel.setBackground(Utils.backgroundColor);
		checkingPanel.setName("LTL Model Checking");
		tarea=new JTextArea();
		tarea.setBackground(Utils.backgroundColor);
		tarea.setSelectionColor(Color.WHITE);
		tarea.setEditable(false);
		element=model.getModelElement("Numerical Temporal Properties");
		checkingPanel.add(tarea,BorderLayout.CENTER);
		JButton clear=new JButton("Clear Screen");
		clear.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
				tarea.setText("");
				
			}});
		JButton save=new JButton("Save");
		save.setActionCommand("saveLTLModelChecking");
		save.addActionListener(this);		
		buttonPanel=new JPanel();
		buttonPanel.setLayout(new FlowLayout());
		buttonPanel.setBackground(Utils.backgroundColor);
		buttonPanel.add(clear);
		buttonPanel.add(save);
		checkingPanel.add(buttonPanel,BorderLayout.LINE_END);
		Utils.modern.setBorderThickness(3);
		Utils.modern.enableAntiAliasing(true);		
			
		ltlModel=new LTLModel(model);
		view=new LTLView(BiochamMainFrame.frame,ltlModel);
	}


	public String getName(int i) {
		return specifications.get(i).getValue();
	}


	public String getValue(int i) {
		return specifications.get(i).getValue();
	}


	public int indexOf(String paramName) {
		 int i=0;
	     while (i<specifications.size() && !getName(i).equals(paramName))
	         i++;
	     if (i == specifications.size())
	         return -1;
	     return i;
	}


	public void resetParameter(String s) {
		
		int i = indexOf(s);
		JFormattedTextField tf = ((JFormattedTextField) panel.getComponent(3*i));
		setSavedResponses(-1);		
		if(!getValue(i).equals(getOrigValues().get(i))){
						
			if (i>=0) {
							
				String ovalue=getOrigValues().get(i);				
			    tf.setValue(ovalue);
			    tf.setName(ovalue);
			    panel.getComponent(3*i+1).setName(ovalue);
			    panel.getComponent(3*i+2).setName(ovalue);
			    tf.setBackground(Color.WHITE);
			    String s1="";
			    if(!isModifying()){
			    	s1="delete_ltl("+s+").\n";
			    }
			    s1+="add_ltl("+ovalue+").\n";
			    setNotToAdd(true);
				model.sendToBiocham(s1,"LTL");	
				specifications.remove(i);
				specifications.add(i,new LTLSpecification(ovalue));
			}
			
		}else{
			 tf.setBackground(Color.WHITE);
		}
		tf=null;
	
	}


	public void setValue(ArrayList list) {
		
		String arg1;
		arg1=(String)list.get(0);	
		int i=indexOf(arg1);
		JFormattedTextField tf=new JFormattedTextField();
		bTip=new BalloonTip(tf,"Don't forget to press ENTER to apply the modification.",Utils.modern,BalloonTip.Orientation.RIGHT_BELOW,BalloonTip.AttachLocation.SOUTHEAST,20,10,false);
		bTip.setText("Don't forget to press ENTER to apply the modification.");
		bTip.setIcon(Icons.icons.get("flag_blue.png"));
		bTip.setIconTextGap(10);	
		bTip.enableClickToHide(true);
		bTip.enableClickToClose(true);		
		tf.addPropertyChangeListener(new PropertyChangeListener(){

			public void propertyChange(PropertyChangeEvent evt) {
				JFormattedTextField tf=(JFormattedTextField)evt.getSource();
				if(tf.isEditable()){
					bTip.setVisible(true);
				}else{
					bTip.setVisible(false);
				}
				
			}});
		tf.addMouseListener(new MouseAdapter(){
	      	  public void mousePressed(MouseEvent e) {	   
	  		   	  if (e.getClickCount() == 2) {
	  		   		resetParameter(e.getComponent().getName());		   		  
	  		   	  }
	  		   }
		});
		
		if (i<0) {
			
			Component[] comps=panel.getComponents();
	        
			for(int k=0;k<comps.length;k++){
				
				if(comps[k].getName().equals("Add")){
	        		panel.remove(k);	       		 	
	        	}
			}		
			specifications.add(new LTLSpecification(arg1));
			ltlModel.addProperty(arg1);
			
			int numParam = specifications.size()-1;
			boolean exists=false;
			for(int y=0;y<getOrigValues().size();y++){
				if(numParam==y){
					exists=true;
					break;
				}
			}
			if(!exists){
				getOrigValues().add(numParam,arg1);
			}			
			SpringLayout layout = (SpringLayout) panel.getLayout();	
			tf.setName(arg1);			
			tf.setValue(arg1);
			tf.setHorizontalAlignment(JTextField.LEFT);
			tf.setColumns(35);
			tf.setEditable(false);
			tf.addKeyListener(new KeyAdapter(){
				
				public void keyPressed(KeyEvent e) {
					   
					Object src=(Component) e.getSource();
					
					if(src instanceof Component){
						Component scrC=(Component)src;
						
						if(scrC instanceof JFormattedTextField){
							if(((JFormattedTextField) scrC).isEditable()){
								JFormattedTextField tf=(JFormattedTextField) scrC;
								scrC=null;
								String name=tf.getName();//parameter's name.....
								
								if(e.getKeyCode()==e.VK_ENTER){
									
								    int i=indexOf(tf.getText());
								    if(i<0){												
												tf.setValue(tf.getText());
												setModified(tf);
												tf.setEditable(false);
												tf.setName(tf.getText());
												setNotToAdd(true);
												lastAdded=tf.getText();
												String s1="delete_ltl("+name+").\n";
												s1+="add_ltl("+tf.getText()+").\n";
												int ind=indexOf(name);
												Component[] comps=panel.getComponents();
												comps[3*ind+1].setName(tf.getText());
												comps[3*ind+2].setName(tf.getText());												
												specifications.remove(ind);
												specifications.add(ind,new LTLSpecification(tf.getText()));
												model.sendToBiocham(s1,"LTL");
												comps=null;
								    }else{//NO		
								    			setModifying(false);
												tf.setValue(name);													
												tf.setEditable(false);
												setNotToAdd(false);
												JOptionPane.showMessageDialog(BiochamMainFrame.frame, "It already exists.");
								    }
								}
							}
						}
					}		
			   }
			});//(model); //On click ENTER call the action that modifies the value
	    	tf.setBackground(Color.white);
	     
	    	JButton but1=new JButton("Modify");
	    	but1.setActionCommand("modify");
	    	but1.setName(arg1);
	    	but1.addActionListener(new ActionListener(){
	    		
	    		public void actionPerformed(ActionEvent e) {
	    			
	    			if(e.getActionCommand()=="modify"){
	    				
	    				setModifying(true);
	    				setSavedResponses(-1);
	    				Component button=(Component) e.getSource();
	    				String name=button.getName();				
	    			    Component[] comps=panel.getComponents();
	    			    Vector cs=new Vector();
	    			    for(int i=0;i<comps.length;i++){
	    			    	if(comps[i].getName().equals(name) && comps[i] instanceof  JFormattedTextField){
	    			    		cs.add(comps[i]);	//label+tf+Modify button....		    		
	    			    	}
	    			    }
	    			    JFormattedTextField tf= (JFormattedTextField) cs.get(0);
	    			    tf.setEditable(true);
	    			    tf.setSelectionColor(Color.GRAY);
	    			    tf.select(0, tf.getText().length());	    			    
	    			    cs.clear();
	    			    cs=null;
	    			    tf=null;
	    			    button=null;
	    			    comps=null;	    			    
	    			}
	    		}
	    	});
	    
	    	JButton but2=new JButton("Delete");
	    	but2.setActionCommand("delete");
	    	but2.setName(arg1);
	    	but2.addActionListener(new ActionListener(){
	    		
	    		public void actionPerformed(ActionEvent e) {
	    			
	    			if(e.getActionCommand()=="delete"){
    				
	    				Component button=(Component) e.getSource();
	    				String name=button.getName();
	    				int k=indexOf(name);
	    				specifications.remove(k);
	    			
	    				getOrigValues().remove(k);
	    				setNotToAdd(true);
	    				String s="delete_ltl("+name+").\n";    				
						model.sendToBiocham(s,"LTL");		
	    			    Component[] comps=panel.getComponents();
	    			    ArrayList cs=new ArrayList();   			    
	    			    for(int i=0;i<comps.length;i++){
	    			    	if(comps[i].getName().equals(name)){
	    			    		cs.add(comps[i]);	    		
	    			    	}
	    			    }	    			      			   
	    			    JFormattedTextField tf= (JFormattedTextField) cs.get(0);
	    			    JButton but1=(JButton)cs.get(1);
	    			    JButton but2=(JButton)cs.get(2);
	    			    panel.remove(tf);
	    			    panel.remove(but1);
	    			    panel.remove(but2);
	    			    comps=null;
	    			    comps=panel.getComponents();
	    			    cs.clear();
	    			    
	    			    for(int i=0;i<comps.length;i++){	
	    			    	if((comps[i] instanceof JButton)){
								if(((JButton)comps[i]).getActionCommand().equals("modify") || ((JButton)comps[i]).getActionCommand().equals("delete") ){
									cs.add(comps[i]);
								}
							}else if((comps[i] instanceof JLabel)){
								if(!((JLabel)comps[i]).getName().equals("refresh")){
									cs.add(comps[i]);
								}
							}else if(!(comps[i] instanceof JButton)){
								cs.add(comps[i]);
							}
	    			    		
	    			    }
	    			    panel.removeAll();
	    			    refreshPanel(cs);	    			    
	    			    cs.clear();	    			        			    
	    			    panel.getParent().validate();
	    				panel.getParent().repaint();
	    				cs=null;
	    			    comps=null;
	    			    button=null;
	    			    tf=null;
	    			    but1=null;
	    			    but2=null;
    			    
	    			}
	    		}
	    	});
	     	    		
	     	    		
	    	panel.add(tf);
	    	panel.add(but1);
	    	panel.add(but2); 
	     
	    	layout.putConstraint(SpringLayout.WEST, tf, LEFT_OFF,SpringLayout.WEST, panel);
	    	layout.putConstraint(SpringLayout.NORTH, tf, TOP_OFF+20+HEIGHT*numParam,SpringLayout.NORTH, panel);
	    	layout.putConstraint(SpringLayout.WEST, but1,10,SpringLayout.EAST, tf);
	     
	    	layout.putConstraint(SpringLayout.NORTH, but1, TOP_OFF+20+HEIGHT*numParam,SpringLayout.NORTH, panel);         
	    	layout.putConstraint(SpringLayout.WEST, but2, 10,SpringLayout.EAST, but1);
	    	
	    	layout.putConstraint(SpringLayout.NORTH, but2, TOP_OFF+20+HEIGHT*numParam,SpringLayout.NORTH, panel);
	     
		}else{
			JOptionPane.showMessageDialog(BiochamMainFrame.frame,"The specification >> "+arg1+" << is already definied.");
		}
		
		if(isNewAdded()){
			refreshAfterAddingNew();
			setNewAdded(false);
		}
		
	}


	public int size() {
		return specifications.size();
	}
	public void setModified(Component comp) {
		comp.setBackground(Utils.modifiedParameterColor);	
		
	}
	
	
	
	
	
public class LTLSpecification{
		
	
	
		private String value;//,origValue;
		
		LTLSpecification(String v){			
			value=v;
		}	

		public String getValue() {
			return value;
		}

		public void setValue(String value) {
			this.value = value;
		}
		
		public String toString() {		  
		      return getValue();
		}		 
	    public void resetValue() {
	    	
	    }		
	}





public JPanel refreshPanel(ArrayList cs) {
	
	
	commandsPanel.removeAll();
	SpringLayout layout = (SpringLayout) panel.getLayout();
	
	int rows = cs.size()/3;	
	for(int i=0;i<cs.size();i++){
		if(cs.get(i) instanceof JButton){
			if(!((JButton)cs.get(i)).getActionCommand().equals("modify") && !((JButton)cs.get(i)).getActionCommand().equals("delete")){
				cs.remove(i);
			}
		}
	}
	rows = cs.size()/3;	
	
	for(int i=0;i<rows;i++){
		 
		JComponent cp=(JComponent) cs.get(3*i);
		
		JFormattedTextField tf=(JFormattedTextField)cp;		
		JButton but1=(JButton)cs.get(3*i+1);
		JButton but2=(JButton)cs.get(3*i+2);			
		panel.add(tf);
		panel.add(but1);
		panel.add(but2);
		
		layout.putConstraint(SpringLayout.WEST, tf, LEFT_OFF, SpringLayout.WEST, panel);
		layout.putConstraint(SpringLayout.NORTH, tf, TOP_OFF+20+HEIGHT*i,SpringLayout.NORTH, panel);		
		layout.putConstraint(SpringLayout.NORTH, but1, TOP_OFF+20+HEIGHT*i,SpringLayout.NORTH, panel);
		layout.putConstraint(SpringLayout.WEST, but1, 10, SpringLayout.EAST, tf);
		layout.putConstraint(SpringLayout.NORTH, but2, TOP_OFF+20+HEIGHT*i,SpringLayout.NORTH, panel);	
		layout.putConstraint(SpringLayout.WEST, but2, 10, SpringLayout.EAST, but1);
		
	}
	
	String toolTipText="<html><i>Adds LTL formula to the current set of LTL properties.<br>" +
			"Example: G(oscil(MA,3)) </i></html>";
	CustomToolTipButton b1=new CustomToolTipButton("Add",toolTipText); 
	b1.setName("Add");
	b1.setActionCommand("addLTL");
    b1.addActionListener(this);
    b1.setBalloonToolTipVisible(false);
	JLabel bi=new JLabel();
	bi.setIcon(Icons.icons.get("Refresh3.png"));
	bi.setName("refresh");	    
	bi.setText("Screen Refresh");
	bi.setForeground(Utils.refreshedColor);
    bi.setToolTipText("Click to Refresh the Screen");
    bi.addMouseListener(new MouseAdapter(){
    	public void mouseClicked(MouseEvent me) {    		              
            refreshAfterAddingNew();	    	
        }
    });  
  
    toolTipText="<html><i>Checks each formula of the LTL specification against<br>" +
    					" a simulation of the given duration. If no duration is provided,<br>" +
    					" tests against the latest simulation.....</i></html>";
	CustomToolTipButton b2=new CustomToolTipButton("Check",toolTipText); 
	b2.setBalloonToolTipVisible(false);
    b2.setName("CheckLTL");
    b2.setActionCommand("checkLTL");
    b2.addActionListener(this);
  
    
    toolTipText="<html><i>Uses the LTL model-checking techniques to automatize<br>" +
    					  "the search for parameter values (kinetic parameters or <br>" +
    					  "initial value parameters) satisfying an LTL specification <br> " +
    					  "with numerical constraints...</i></html>";
    CustomToolTipButton b3=new CustomToolTipButton("Search Parameters",toolTipText);      
    b3.setName("searchParameters");
    b3.setActionCommand("searchParams");
    b3.addActionListener(this);
    b3.setBalloonToolTipVisible(false);
    
    
    toolTipText="<html><i>Computes domains of variables contained in the <br>" +
    					 "specified QFLTL query that makes the formula true <br>" +
    					 "on a numerical trace. <br>" +
    					 "The trace used is the last simulated or loaded trace.</i></html>";
    CustomToolTipButton b4=new CustomToolTipButton("Check LTL Property",toolTipText);       
    b4.setName("traceAnalyze");
    b4.setActionCommand("traceAnalyze");
    b4.addActionListener(this);
    b4.setBalloonToolTipVisible(false);
    
    
    toolTipText="<html><i>LTL(R) formulae are evaluated on a numerical trace either created by simulation or imported from outside with this command...</i></html>";
    CustomToolTipButton b5=new CustomToolTipButton("Load Trace",toolTipText);      
    b5.setName("loadTrace");
    b5.setActionCommand("loadTrace");   
    b5.addActionListener(this);
    b5.setBalloonToolTipVisible(false);
    
    
    toolTipText="<html><i>Compute the satisfaction degree of a QFLTL(R) formula given with the list of its free variables and the list of the objective values.</i></html>";
    CustomToolTipButton b6=new CustomToolTipButton("Compute Satisfaction degree",toolTipText);      
    b6.setName("satisfactionDegree");
    b6.setActionCommand("satisfactionDegree");   
    b6.addActionListener(this);
    b6.setBalloonToolTipVisible(false);
    
    toolTipText="<html><i>Compute the satisfaction degree landscape of a QFLTL(R) formula (third argument with list of variables and objective values \n" +
    		" in fourth and fifth argument respectively) on a 2D parameter grid saved in a .csv file (last argument). The grid is defined \n " +
    		"by a list of two parameters (first argument) given with their range (second argument) scanned with a fixed step (sixth argument).\n" +
    		" The seventh argument specifies the time horizon for the simulation.</i></html>";
    CustomToolTipButton b7=new CustomToolTipButton("Get Satisfaction landscape",toolTipText);      
    b7.setName("landscape");
    b7.setActionCommand("landscape");   
    b7.addActionListener(this);
    b7.setBalloonToolTipVisible(false);
    
    toolTipText="<html><i>Compute the robustness of the model with respect to a given QFLTL(R) property and a parameter perturbation model.</i></html>";
    CustomToolTipButton b8=new CustomToolTipButton("Compute Robustness",toolTipText);      
    b8.setName("robustness");
    b8.setActionCommand("robustness");   
    b8.addActionListener(this);
    b8.setBalloonToolTipVisible(false);

    
    
	Component[] comps=panel.getComponents();
	
	if(comps.length>0 && !(comps[0] instanceof JButton)){
		
		 int size=comps.length/3;
		 
		 panel.add(b1);
		 panel.add(bi);
		 
		 commandsPanel.add(b2);
		 commandsPanel.add(b4);	
		 commandsPanel.add(b6);
		 commandsPanel.add(b7);
		 commandsPanel.add(b8);
		 commandsPanel.add(b3);		 
		 commandsPanel.add(b5);	 
		 
		 
		 layout.putConstraint(SpringLayout.NORTH, b1, BiochamModel.TOP_OFF+BiochamModel.HEIGHT*(size+3),SpringLayout.NORTH, panel);	
		 layout.putConstraint(SpringLayout.NORTH, bi, BiochamModel.TOP_OFF+BiochamModel.HEIGHT*(size+3),SpringLayout.NORTH, panel);
		 Spring con=layout.getConstraint(SpringLayout.WEST,panel.getComponent(3*(size-1)+1));
		 layout.putConstraint(SpringLayout.WEST, b1, con, SpringLayout.WEST, panel);
		 layout.putConstraint(SpringLayout.WEST, bi, LEFT_OFF+10, SpringLayout.EAST, b1);		 
        
		 sMC.putConstraint(SpringLayout.WEST, b2, LEFT_OFF+10, SpringLayout.WEST, commandsPanel);
		 sMC.putConstraint(SpringLayout.WEST, b3, LEFT_OFF+10, SpringLayout.WEST, commandsPanel);
		 sMC.putConstraint(SpringLayout.WEST, b4, LEFT_OFF+10, SpringLayout.WEST, commandsPanel);
		 sMC.putConstraint(SpringLayout.WEST, b5, LEFT_OFF+10, SpringLayout.WEST, commandsPanel);
		 sMC.putConstraint(SpringLayout.WEST, b6, LEFT_OFF+10, SpringLayout.WEST, commandsPanel);
		 sMC.putConstraint(SpringLayout.WEST, b7, LEFT_OFF+10, SpringLayout.WEST, commandsPanel);
		 sMC.putConstraint(SpringLayout.WEST, b8, LEFT_OFF+10, SpringLayout.WEST, commandsPanel);
		 sMC.putConstraint(SpringLayout.NORTH, b2, 10,SpringLayout.NORTH, commandsPanel);
         sMC.putConstraint(SpringLayout.NORTH, b4, 20,SpringLayout.SOUTH, b2);
         
         sMC.putConstraint(SpringLayout.NORTH, b6, 20,SpringLayout.SOUTH, b4);
         sMC.putConstraint(SpringLayout.NORTH, b7, 20,SpringLayout.SOUTH, b6);
         sMC.putConstraint(SpringLayout.NORTH, b8, 20,SpringLayout.SOUTH, b7);
         
         sMC.putConstraint(SpringLayout.NORTH, b3, 20,SpringLayout.SOUTH, b8);
         sMC.putConstraint(SpringLayout.NORTH, b5, 20,SpringLayout.SOUTH, b3);
		 
	}else{
		
		 panel.add(b1);		
		 panel.add(bi);
		 commandsPanel.add(b4);	
		 commandsPanel.add(b6);
		 commandsPanel.add(b7);
		 commandsPanel.add(b8);
		 commandsPanel.add(b3);		 
		 commandsPanel.add(b5);	 
		 
		 layout.putConstraint(SpringLayout.NORTH, b1, HEIGHT,SpringLayout.NORTH, panel);
		 layout.putConstraint(SpringLayout.WEST, b1, 50, SpringLayout.WEST, panel);
		 layout.putConstraint(SpringLayout.NORTH, bi, HEIGHT,SpringLayout.NORTH, panel);
		 layout.putConstraint(SpringLayout.WEST, bi, LEFT_OFF+10, SpringLayout.EAST, b1);
	
		 
		 sMC.putConstraint(SpringLayout.WEST, b3, LEFT_OFF+10, SpringLayout.WEST, commandsPanel);
		 sMC.putConstraint(SpringLayout.WEST, b4, LEFT_OFF+10, SpringLayout.WEST, commandsPanel);
		 sMC.putConstraint(SpringLayout.WEST, b5, LEFT_OFF+10, SpringLayout.WEST, commandsPanel);
		 sMC.putConstraint(SpringLayout.WEST, b6, LEFT_OFF+10, SpringLayout.WEST, commandsPanel);
		 sMC.putConstraint(SpringLayout.WEST, b7, LEFT_OFF+10, SpringLayout.WEST, commandsPanel);
		 sMC.putConstraint(SpringLayout.WEST, b8, LEFT_OFF+10, SpringLayout.WEST, commandsPanel);
		 sMC.putConstraint(SpringLayout.WEST, b5, LEFT_OFF+10, SpringLayout.WEST, commandsPanel);
		 sMC.putConstraint(SpringLayout.NORTH, b4, 10,SpringLayout.NORTH, commandsPanel);
		 
		 sMC.putConstraint(SpringLayout.NORTH, b6, 20,SpringLayout.SOUTH, b4);
		 sMC.putConstraint(SpringLayout.NORTH, b7, 20,SpringLayout.SOUTH, b6);
		 sMC.putConstraint(SpringLayout.NORTH, b8, 20,SpringLayout.SOUTH, b7);
	         
         sMC.putConstraint(SpringLayout.NORTH, b3, 20,SpringLayout.SOUTH, b8);
         sMC.putConstraint(SpringLayout.NORTH, b5, 20,SpringLayout.SOUTH, b3);
		 
		
	}	
		
    cs.clear();
    commandsPanel.revalidate();
    panel.revalidate();
	
	
	return panel;
	
	
}


/**
 * 
 */
public void refreshAfterAddingNew() {
	
	ArrayList<Component> cs=new ArrayList<Component>();
	Component[] comps=panel.getComponents();
	for(int i=0;i<comps.length;i++){
		if((comps[i] instanceof JButton)){
			if(!((JButton)comps[i]).getActionCommand().equals("addLTL") && !((JButton)comps[i]).getActionCommand().equals("checkLTL") 
					&& !((JButton)comps[i]).getActionCommand().equals("searchParams") && !((JButton)comps[i]).getActionCommand().equals("traceAnalyze")){
				
				cs.add(comps[i]);
			}
		}else if(comps[i] instanceof JLabel){
			if(!comps[i].getName().equals("refresh")){
				cs.add(comps[i]);
			}
		}else{
			cs.add(comps[i]);
		}
	}
	panel.removeAll();			
	panel=refreshPanel(cs);			
	cs.clear();
	cs=null;
	comps=null;
	int numComps1=panel.getComponentCount();
	int dynHeight1=(numComps1/2)*40*2;				
	panel.setPreferredSize(new Dimension(panel.getSize().width,dynHeight1+300));
	panel.revalidate();
}

public JPanel getLTLModelChecking() {	
	 return checkingPanel;	 
}


public int getSavedResponses() {
	return savedResponses;
}


public void setSavedResponses(int savedResponses) {
	this.savedResponses = savedResponses;
}


public void resetSavedResponses() {
	savedResponses=-1;
}


public void disposeElements() {
	
	specifications.clear();
	specifications=null;
	biocham=null;
	panel=null;
	model=null;
	element=null;
	savedResponses=0;
}


public boolean isNotToAdd() {
	return notToAdd;
}


public void setNotToAdd(boolean notToAdd) {
	this.notToAdd = notToAdd;
}


public ArrayList<String> getOrigValues() {
	return origValues;
}


public JTextArea getTarea() {
	return tarea;
}


public void setTarea(JTextArea tarea) {
	this.tarea = tarea;
}


public boolean isFromBeggining() {
	return fromBeggining;
}


public void setFromBeggining(boolean fromBeggining) {
	this.fromBeggining = fromBeggining;
}


public boolean isCheck() {
	return check;
}


public void setCheck(boolean search) {
	this.check = search;
}


public void actionPerformed(ActionEvent e) {

	if(e.getSource() instanceof CustomToolTipButton){
		CustomToolTipButton b=(CustomToolTipButton)e.getSource();
		b.setBalloonToolTipVisible(false);
	}
	
	
	if(e.getActionCommand().equals("saveLTLModelChecking")){
		saveLTLModelChecking();
	}
	
	else if(e.getActionCommand()=="addLTL"){
		
		addParameter();
	}
	
	else if(e.getActionCommand()=="checkLTL"){				
		
		DialogOptions od=new DialogOptions(BiochamMainFrame.frame,model.getModelElement("Numerical Temporal Properties"),"Check LTL",panel);
		String cmd="";
		Object[] newParam=new Object[2];
		newParam=od.getNewParameter();
		String name=(String)newParam[0];
		String value=(String)newParam[1];
		if(value!=null){
			od=null;
			newParam=null;	
			Utils.debugMsg("Value is: "+value);
			if(name!=null && name!="" && !name.equals(null) && name.length()>0){
				//model.createModelBackup();
				if(name.length()>0){
					cmd="check_ltl_spec("+name+").\n";
					model.simulationMap.put("checkLTL", name);
				}else{
					cmd="check_ltl_spec.\n";
				}
				setFromBeggining(true);
				setCheck(true);
				if(cmd!=null){
					tarea.append("\n\n Checking LTL specifications.......\n");
					model.sendToBiocham(cmd,"LTL");
				}
			}else{
				cmd="check_ltl_spec.\n";
				setFromBeggining(true);
				setCheck(true);
				if(cmd!=null){
					tarea.append("\n\n Checking LTL specifications.......\n");
					model.sendToBiocham(cmd,"LTL");
				}
			}
		}
		
		//checkLTL();		
	}		
	else if(e.getActionCommand()=="searchParams"){		
		
		searchParams();
	}
	else if(e.getActionCommand()=="robustness"){
		DialogSearchParams spd=new DialogSearchParams(BiochamMainFrame.frame,model.getModelElement("Numerical Temporal Properties"),"Robustness",panel);
		//model.createModelBackup();
		String cmd=spd.getResult();
		setCheck(false);
		setFromBeggining(true);
		if(cmd!=null){
			tarea.append("\n\n Computing robustness started.............");
			tarea.append("\n\n It is not recommended that you start another task until this is not finished.\n If you want to stop this task, click on <Stop Execution> from the toolbar.\n\n");
			model.sendToBiocham(cmd,"LTL");
		}
		cmd=null;
		spd=null;
		
	}else if(e.getActionCommand()=="satisfactionDegree"){
		
		SatisfactionDegreeDialog dg=new SatisfactionDegreeDialog(BiochamMainFrame.frame,model.getModelElement("Numerical Temporal Properties"));
		String cmd=dg.getCommand();
		if(cmd!=null){
			tarea.append("\n\n Computing satisfaction degree.............");
			tarea.append("\n\n It is not recommended that you start another task until this is not finished.\n If you want to stop this task, click on <Stop Execution> from the toolbar.\n\n");
			model.sendToBiocham(cmd,"LTL");
		}
		cmd=null;
		dg=null;
		
	}else if(e.getActionCommand()=="landscape"){
		DialogSearchParams spd=new DialogSearchParams(BiochamMainFrame.frame,model.getModelElement("Numerical Temporal Properties"),"Satisfaction Degree Landscape",panel);	
		//model.createModelBackup();
		String cmd=spd.getResult();
		setCheck(false);
		setFromBeggining(true);
		if(cmd!=null){
			tarea.append("\n\n Computing satisfaction degree landscape started.............");
			tarea.append("\n\n It is not recommended that you start another task until this is not finished.\n If you want to stop this task, click on <Stop Execution> from the toolbar.\n\n");
			model.sendToBiocham(cmd,"LTL");
		}
		cmd=null;
		spd=null;
	}
	else if(e.getActionCommand()=="traceAnalyze"){				
    	
		traceAnalyze();
	}
	else if(e.getActionCommand()=="loadTrace"){	
			Utils.fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
			String rep=Utils.showOpenDialog(BiochamMainFrame.frame,SupportedSuffixes.CSV_SUFFIX);			
			if (rep!=null) {
				final File file=new File(rep);
				if(!file.isDirectory()){
		            String s = "load_trace('"+ file.getPath() +"').\n";
		            model.sendToBiocham(s,"LTL");
				}else{
					JOptionPane.showMessageDialog(BiochamMainFrame.frame,"Select a file, not a whole directory!", "Selection Warning",JOptionPane.WARNING_MESSAGE);
				}
			}
	}else if(e.getActionCommand().equals("deleteParameters")){
		model.sendToBiocham("clear_ltl.\n");
		this.specifications.clear();
	
		ArrayList cs=new ArrayList(0);
		panel.removeAll();			
		panel=refreshPanel(cs);			
		cs.clear();
		cs=null;		
		int numComps1=panel.getComponentCount();
		int dynHeight1=(numComps1/2)*40*2;				
		panel.setPreferredSize(new Dimension(panel.getSize().width,dynHeight1+300));
		panel.revalidate();
	}else{
		String cmd=e.getActionCommand();
		((ParamTableParameters)model.getParameters().getParamTable()).setDontAskForModify(false);
		foundParameters.clear();
		setCmdFinished(false);
		setFound(false);		
		DialogSearchParams spd=null;
		//model.createModelBackup();
		if(cmd!=null){
			
			if(cmd.equals("search_parameters")){
				spd=new DialogSearchParams(BiochamMainFrame.frame,model.getModelElement("Numerical Temporal Properties"),"Search Parameters",panel);						
			}else if(cmd.equals("search_all_parameters")){
				spd=new DialogSearchParams(BiochamMainFrame.frame,model.getModelElement("Numerical Temporal Properties"),"Search All Parameters",panel);						
			}else if(cmd.equals("search_random_parameters")){
				spd=new DialogSearchParams(BiochamMainFrame.frame,model.getModelElement("Numerical Temporal Properties"),"Search Random Parameters",panel);
			}else if(cmd.equals("search_random_all_parameters")){
				spd=new DialogSearchParams(BiochamMainFrame.frame,model.getModelElement("Numerical Temporal Properties"),"Search Random All Parameters",panel);
			}else if(cmd.equals("search_parameters_cmaes")){
				spd=new DialogSearchParams(BiochamMainFrame.frame,model.getModelElement("Numerical Temporal Properties"),"Search Parameters CMAES",panel);
			}else if(cmd.equals("robustness")){
				spd=new DialogSearchParams(BiochamMainFrame.frame,model.getModelElement("Numerical Temporal Properties"),"Robustness",panel);						
			}else if(cmd.equals("landscape")){
				spd=new DialogSearchParams(BiochamMainFrame.frame,model.getModelElement("Numerical Temporal Properties"),"Landscape",panel);			
			}else if(cmd.equals("cmaes_multi_conditions")){
				spd=new DialogSearchParams(BiochamMainFrame.frame,model.getModelElement("Numerical Temporal Properties"),"Search Parameters CMAES Multi Conditions",panel);
			}	
			
			cmd=spd.getResult();
			setCheck(false);
			setFromBeggining(true);
			if(cmd!=null){
				this.setStopPrinting(false);
				tarea.append("\n\n Searching started.............");		
				model.sendToBiocham(cmd,"LTL");
			}
		}
	}
	
}


public void traceAnalyze() {
	BiochamModelElement el=model.getModelElement("Numerical Temporal Properties");
	//el.getModel().createModelBackup();
	DialogOptions od=new DialogOptions(BiochamMainFrame.frame,el,"Check LTL Property",panel);
	String cmd="";
	Object[] newParam=new Object[2];
	newParam=od.getNewParameter();	
	String value=(String)newParam[1];
	od=null;
	newParam=null;	
	if(value!=null && value!="" && !value.equals(null) && value.length()>0){
		value=value.trim();
		cmd="domains("+value.trim()+").\n";
		setCheck(false);
		setFromBeggining(true);
		if(cmd!=null){
			tarea.append("\n\n Analyzing trace started.........");
			tarea.append("\nChecking the LTL property.........");
			model.sendToBiocham(cmd,"LTL");
		}
	}	
}


public void searchParams() {
	
	((ParamTableParameters)model.getParameters().getParamTable()).setDontAskForModify(false);
	foundParameters.clear();
	setCmdFinished(false);
	setFound(false);
	
	DialogOptions od=new DialogOptions(BiochamMainFrame.frame,model.getModelElement("Numerical Temporal Properties"),"Search Parameters",panel);
	String cmd=od.getNewOneParameter();
	DialogSearchParams spd=null;
	//model.createModelBackup();
	if(cmd!=null){
		
		if(cmd.equals("search_parameters")){
			spd=new DialogSearchParams(BiochamMainFrame.frame,model.getModelElement("Numerical Temporal Properties"),"Search Parameters",panel);						
		}else if(cmd.equals("search_all_parameters")){
			spd=new DialogSearchParams(BiochamMainFrame.frame,model.getModelElement("Numerical Temporal Properties"),"Search All Parameters",panel);						
		}else if(cmd.equals("search_random_parameters")){
			spd=new DialogSearchParams(BiochamMainFrame.frame,model.getModelElement("Numerical Temporal Properties"),"Search Random Parameters",panel);
		}else if(cmd.equals("search_random_all_parameters")){
			spd=new DialogSearchParams(BiochamMainFrame.frame,model.getModelElement("Numerical Temporal Properties"),"Search Random All Parameters",panel);
		}else if(cmd.equals("search_parameters_cmaes")){
			spd=new DialogSearchParams(BiochamMainFrame.frame,model.getModelElement("Numerical Temporal Properties"),"Search Parameters CMAES",panel);
		}else if(cmd.equals("robustness")){
			spd=new DialogSearchParams(BiochamMainFrame.frame,model.getModelElement("Numerical Temporal Properties"),"Robustness",panel);						
		}else if(cmd.equals("landscape")){
			spd=new DialogSearchParams(BiochamMainFrame.frame,model.getModelElement("Numerical Temporal Properties"),"Landscape",panel);			
		}else if(cmd.equals("cmaes_multi_conditions")){
			spd=new DialogSearchParams(BiochamMainFrame.frame,model.getModelElement("Numerical Temporal Properties"),"Search Parameters CMAES Multi Conditions",panel);
		}	
		
		cmd=spd.getResult();
		setCheck(false);
		setFromBeggining(true);
		if(cmd!=null){
			this.setStopPrinting(false);
			tarea.append("\n\n Searching started.............");		
			model.sendToBiocham(cmd,"LTL");
		}
	}
}


/*public void checkLTL() {
	model.createModelBackup();
	String cmd="";
	String response="";
	response= (String) JOptionPane.showInputDialog(BiochamMainFrame.frame," Do you want to specify a simulation duration?\n\n",
			  "Simulation Duration",
			  JOptionPane.QUESTION_MESSAGE,null,null,DialogOptions.map.get("checkLTL"));
	if(response!=null){
		if(response.length()>0){
			cmd="check_ltl_spec("+response+").\n";
			DialogOptions.map.put("checkLTL", response);
		}else{
			cmd="check_ltl_spec.\n";
		}
		setFromBeggining(true);
		setCheck(true);
		if(cmd!=null){
			tarea.append("\n\n Checking LTL specifications.......\n");
			model.sendToBiocham(cmd,"LTL");
		}
	}
}*/


public void addParameter() {
	
	setNewAdded(true);
	setNotToAdd(false);
	setSavedResponses(-1);	
	element=model.getLtlSpecifications();
	DialogAddSpecification params=new DialogAddSpecification(BiochamMainFrame.frame, element, "LTL Operators",panel);

	String value=params.getFormula();
	
	if(value!=null && value!=""){
		
		String s="";
		if(value.contains("curve_fit")){					
					s="add_ltl("+value+").\n";	    					
					model.sendToBiocham(s,"LTL");
		}else{
		
		int i=indexOf(value);
			if(i<0){    
				
				s="add_ltl("+value+").\n";	    					
				model.sendToBiocham(s,"LTL");
			}else{
				JOptionPane.showMessageDialog(BiochamMainFrame.frame,"The specification  "+value+"  is already definied.");
			}
		}
	}
}


public void saveLTLModelChecking() {
	final String text=tarea.getText();
	Utils.fileChooser.setDialogTitle("Save file as");
	Utils.fileChooser.setDialogType(JFileChooser.SAVE_DIALOG);
	Utils.fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
	String rep=Utils.showSaveDialog("",BiochamMainFrame.frame,SupportedSuffixes.TEXT_SUFFIX);			
	if (rep!=null) {
          
	   final String dir=(new File(rep)).getAbsolutePath();
	   SwingWorker sw=new SwingWorker(){

		@Override
		public Object construct() {
			try{
			    // Create file 
			    FileWriter fstream = new FileWriter(dir);
			    BufferedWriter out = new BufferedWriter(fstream);
			    out.write(text);
			    //Close the tarea stream
			    out.close();
			    }catch (Exception ex){//Catch exception if any
			    	JOptionPane.showMessageDialog(BiochamMainFrame.frame, ex.getMessage());
			      System.err.println("Error: " + ex.getMessage());
			    }			
			return null;
		}

		@Override
		public void finished() {
			// TODO Auto-generated method stub
			
		}
	   };
	   sw.start();   
	   	  
	}
}


public void removeLastAdded() {
	
	resetParameter(lastAdded);
	
}


public boolean isModifying() {
	return modifying;
}


public void setModifying(boolean modifying) {
	this.modifying = modifying;
}


private String getLastAdded() {
	return lastAdded;
}


private void setLastAdded(String lastAdded) {
	this.lastAdded = lastAdded;
}


public Vector<LTLSpecification> getSpecifications() {
	return specifications;
}


public JPanel getCommandsPanel() {
	return commandsPanel;
}


public void setStopPrinting(boolean b) {
	// TODO Auto-generated method stub
	
}


public boolean isStopPrinting() {
	return stopPrinting;
}


public HashMap<String, String> getFoundParameters() {
	return foundParameters;
}


public void setFound(boolean b) {
	found=true;
	
}


public boolean isCmdFinished() {
	return cmdFinished;
}


public void setCmdFinished(boolean cmdFinished) {
	this.cmdFinished = cmdFinished;
}




}
