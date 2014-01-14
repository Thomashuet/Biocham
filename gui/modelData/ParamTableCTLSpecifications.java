package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.BiochamModelElement;
import fr.inria.contraintes.biocham.WorkbenchArea;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.customComponents.DisabledGlassPane;
import fr.inria.contraintes.biocham.dialogs.CustomInputDialog;
import fr.inria.contraintes.biocham.dialogs.DialogAddSpecification;
import fr.inria.contraintes.biocham.dialogs.DialogOptions;
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
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.Spring;
import javax.swing.SpringLayout;
import javax.swing.SwingUtilities;




public class ParamTableCTLSpecifications implements Parameters, ActionListener{

	
	private Vector<CTLSpecification> specifications;
	WorkbenchArea biocham;
	JPanel panel;
	BiochamModel model;
	BiochamModelElement element;
	int savedResponses;
	ArrayList<String> origValues;
	ArrayList<String> check;
	boolean deleting=false;
	boolean notToAdd=false;
	JPanel checkingPanel, commandsPanel;
	SpringLayout commandsPanelLayout;	
	int north=0;
	private boolean forChecking=true;
	JTextArea tarea;
	JPanel buttonPanel;
	boolean revising=false;
	BalloonTip bTip;
	boolean newAdded=false;
	boolean addingGenerated=false;
	boolean glassPaneAdded=false;
	DisabledGlassPane glassPane=null;
	JRootPane rootPane=null;
	boolean modifying=false;
	String lastAdded="";
	JCheckBox whyOption,reorderingOption,modeOption,fairnessOption;
	
	CTLModel ctlModel;
	CTLView view;
	
	
	public CTLModel getCtlModel() {
		return ctlModel;
	}
	public void setCtlModel(CTLModel ctlModel) {
		this.ctlModel = ctlModel;
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
	
	public int getNorth() {
		return north;
	}


	public void setNorth(int north) {
		this.north = north;
	}


	public ParamTableCTLSpecifications(BiochamModel m,WorkbenchArea workbench, JPanel p){
		
		
		biocham=workbench;
		model=m;
		panel=p;
		setSpecifications(new Vector<CTLSpecification>());
		savedResponses=-1;
		origValues=new ArrayList<String>();
		check=new ArrayList<String>();
		
		commandsPanel=new JPanel();			 
		commandsPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createLineBorder(Color.black),"Checking CTL Properties"));
		commandsPanel.setBackground(Utils.backgroundColor);
		commandsPanel.setName("Checking CTL Properties");
		commandsPanelLayout=new SpringLayout();
		commandsPanel.setLayout(commandsPanelLayout);
		
		checkingPanel=new JPanel();			 
		checkingPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createLineBorder(Color.black),"Results"));
		checkingPanel.setLayout(new BorderLayout());	
		checkingPanel.setBackground(Utils.backgroundColor);		
		checkingPanel.setName("CTL Model Checking");
		tarea=new JTextArea();
		tarea.setSelectionColor(Color.WHITE);
		tarea.setBackground(Utils.backgroundColor);
		tarea.setEditable(false);
		checkingPanel.add(tarea,BorderLayout.CENTER);
		JButton clear=new JButton("Clear Screen");
		clear.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
				tarea.setText("");
				
			}});
		JButton save=new JButton("Save");
		save.setActionCommand("saveCTLModelChecking");
		save.addActionListener(this);
		buttonPanel=new JPanel();
		buttonPanel.setLayout(new FlowLayout());
		buttonPanel.setBackground(Utils.backgroundColor);
		buttonPanel.add(clear);
		buttonPanel.add(save);
		checkingPanel.add(buttonPanel,BorderLayout.LINE_END);
		Utils.modern.setBorderThickness(3);
		Utils.modern.enableAntiAliasing(true);
		glassPane = new DisabledGlassPane();
		
		
		ctlModel=new CTLModel(model);
		view=new CTLView(BiochamMainFrame.frame,ctlModel);
	}



	
	public String getName(int i) {
		return getSpecifications().get(i).getValue();
	}


	public String getValue(int i) {
		return getSpecifications().get(i).getValue();
	}


	public int indexOf(String paramName) {
		 int i=0;
	     while (i<getSpecifications().size() && !getName(i).equals(paramName))
	         i++;
	     if (i == getSpecifications().size())
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
			     s1="delete_spec("+s+").\n";
			    }
			    s1+="add_spec("+ovalue+").\n";
			    setNotToAdd(true);
				model.sendToBiocham(s1,"CTL");	
				getSpecifications().remove(i);
				getSpecifications().add(i,new CTLSpecification(ovalue));
				
			}
		}else{
			 tf.setBackground(Color.WHITE);
		}
		tf=null;
	}

	public void checkAddingGenerated(){
		if(isAddingGenerated()){
			if(!glassPaneAdded){				
				rootPane = SwingUtilities.getRootPane(panel);
				rootPane.setGlassPane( glassPane );				
				glassPane.activate("Execution in progress.....Please Wait...");
				glassPaneAdded=true;
				panel.setVisible(false);
			}			
		}else{			
			
			//refreshCTLSpecifications();
			setNewAdded(false);	
			if(glassPaneAdded){
				panel.setVisible(true);				
				glassPane.deactivate();
				glassPaneAdded=false;
			}
		}
	}
	public void setValue(ArrayList list) {
		
		
		String arg1;
		arg1=(String)list.get(0);	
		int i=indexOf(arg1);
		
		JFormattedTextField tf=new JFormattedTextField();	
		bTip=new BalloonTip(tf,"Don't forget to press ENTER to apply the modification.",Utils.modern,BalloonTip.Orientation.RIGHT_ABOVE,BalloonTip.AttachLocation.NORTHEAST,20,10,false);
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
			getSpecifications().add(new CTLSpecification(arg1)); //vector of parameters
			ctlModel.addProperty(arg1);
			
			 
			int numParam = getSpecifications().size()-1;		
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

			Spring maxSpring = Spring.constant(MIDDLE);
			SpringLayout layout = (SpringLayout) panel.getLayout();	

			tf.setName(arg1);			
			tf.setValue(arg1);
			tf.setColumns(35);
			tf.setHorizontalAlignment(JTextField.LEFT);
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
												tf.setName(tf.getText());
												setModified(tf);
												tf.setEditable(false);
												setNotToAdd(true);
												lastAdded=tf.getText();
												String s1="delete_spec("+name+").\n";
												s1+="add_spec("+tf.getText()+").\n";												
												int ind=indexOf(name);
												Component[] comps=panel.getComponents();
												comps[3*ind+1].setName(tf.getText());
												comps[3*ind+2].setName(tf.getText());
												getSpecifications().remove(ind);
												getSpecifications().add(ind,new CTLSpecification(tf.getText()));
												model.sendToBiocham(s1,"CTL");
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
	    				getSpecifications().remove(k);
	    				getOrigValues().remove(k);
	    				setNotToAdd(true);
	    				String s="delete_spec("+name+").\n";    				
						model.sendToBiocham(s,"CTL");		
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
	    	layout.putConstraint(SpringLayout.NORTH, tf, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);
	    	layout.putConstraint(SpringLayout.EAST, tf, -10,SpringLayout.WEST, but1);	     
	    	layout.putConstraint(SpringLayout.NORTH, but1, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);         
	    	layout.putConstraint(SpringLayout.EAST, but1, -10,SpringLayout.WEST, but2);	    	
	    	layout.putConstraint(SpringLayout.NORTH, but2, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);
	     	    
	    	for (i=0;i<getSpecifications().size();++i)
	    		maxSpring = Spring.max(maxSpring, Spring.sum(layout.getConstraints(panel.getComponent(3*i)).getWidth(),Spring.constant(10)));
	    	for (i=0;i<getSpecifications().size();++i)
	    		layout.putConstraint(SpringLayout.WEST, panel.getComponent(3*i+1), maxSpring, SpringLayout.WEST, panel);	     
		}else{
			//JOptionPane.showMessageDialog(BiochamMainFrame.frame,"The specification >> "+arg1+" << is already definied.");
		}
		
		if(isNewAdded() && !isAddingGenerated()){
			refreshCTLSpecifications();
			setNewAdded(false);
		}
		
	}


	public int size() {
		return getSpecifications().size();
	}
	public void setModified(Component comp) {
		comp.setBackground(Utils.modifiedParameterColor);	
		
	}
	
	
	
	
	
public class CTLSpecification{
		
	
	
		private String value,origValue;
		
		CTLSpecification(String v){			
			origValue=new String(v);
			value=origValue;
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
		 public String getOrigValue() {  		
	    	  return origValue;
	      }
	    public void resetValue() {
	    	setValue(origValue);
	    }

	    /**
	     * Returns true or false depending if the current value is different
	     * from the original value.
	     */     
	    public boolean hasChanged() {
	    	return (! value.equals(origValue));
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
		layout.putConstraint(SpringLayout.NORTH, tf, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);		
		layout.putConstraint(SpringLayout.NORTH, but1, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);
		layout.putConstraint(SpringLayout.WEST, but1, 10, SpringLayout.EAST, tf);
		layout.putConstraint(SpringLayout.NORTH, but2, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);	
		layout.putConstraint(SpringLayout.WEST, but2, 10, SpringLayout.EAST, but1);
			   
	}
	 
    String toolTipText="<html><i>Add CTL temporal properties to the current set of CTL properties.<br>" +
    		"Example: Ei(reachable(!(MB))) </i></html>";
    CustomToolTipButton b1=new CustomToolTipButton("Add",toolTipText); 
    b1.setName("Add");
    b1.setActionCommand("addCTL");
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
            refreshCTLSpecifications();	    	
        }
    });
    
    toolTipText="<html><i>Adds to the current specification those simple CTL properties <br>" +
    					  "(reachable, oscil, steady, checkpoint for each molecule) that <br>" +
    					  "are true in the model.</i></html>";
    CustomToolTipButton b2=new CustomToolTipButton("Add generated CTL",toolTipText); 
    b2.setName("AddGenCTL");
    b2.setActionCommand("addGenCTL");
    b2.addActionListener(this);		
    b2.setBalloonToolTipVisible(false);
  
    toolTipText="<html><i>Automatically generates a set of all CTL properties of some simple pattern that are true in the model.</i></html>";
    CustomToolTipButton b02=new CustomToolTipButton("Generate CTL",toolTipText); 
    b02.setName("genCTL");
    b02.setActionCommand("genCTL");
    b02.addActionListener(this);		
    b02.setBalloonToolTipVisible(false);
    
    toolTipText="<html><i>Tests the adequacy of the model w.r.t. its specification,<br>" +
    					  "and either for each unsatisfied CTL property, computes the <br>" +
    					  "result of why[CHECK], or for each CTL property, computes the <br>" +
    					  "result of why[CHECK_WHY], or summarizes the result with the first <br>" +
    					  "unsatisfied property if there is one[CHECK_ALL]. </i></html>";
    CustomToolTipButton b3=new CustomToolTipButton("Check",toolTipText); 
    b3.setName("CheckCTL");
    b3.setActionCommand("checkCTL");
    b3.addActionListener(this);
    b3.setBalloonToolTipVisible(false);
  
    toolTipText="<html><i>Evaluates a temporal query using the model-checker NuSMV.<br>" +
    					  "The first use of this command may take a while as it will <br>" +
    					  "compile the rules into an ordered binary decison diagram (OBDD).</i></html>";
    CustomToolTipButton b4=new CustomToolTipButton("Check CTL Property",toolTipText);   
    b4.setName("Nusmv");
    b4.setActionCommand("nusmv");
    b4.addActionListener(this);
    b4.setBalloonToolTipVisible(false);
    
   
    toolTipText="<html><i>Reduces the model by deleting rules, up to a minimal model <br>" +
    					  "that satisfies the whole specification.</i></html>";
    CustomToolTipButton b5=new CustomToolTipButton("Reduce Model",toolTipText);  
    b5.setName("reduceModel");
    b5.setActionCommand("reduceModel");
    b5.addActionListener(this);
	b5.setBalloonToolTipVisible(false);
	   
    toolTipText="<html><i>Uses Machine Learning techniques to try and find completions or modifications <br>" +
    					  "of a model such that the specification is satisfied.....</i></html>";
    CustomToolTipButton b6=new CustomToolTipButton("Learn Rules",toolTipText);  
    b6.setName("learnRules");
    b6.setActionCommand("learnRules");
    b6.addActionListener(this);
    b6.setBalloonToolTipVisible(false);
  
    
    toolTipText="<html><i>Tries to correct the model using a theory revision algorithm...</i></html>";
    CustomToolTipButton b7=new CustomToolTipButton("Revise Model",toolTipText); 
    b7.setBalloonToolTipVisible(false);
    b7.setName("reviseModel");
    b7.setActionCommand("reviseModel");
    b7.addActionListener(this); 
  
    whyOption=new JCheckBox("Why");
    whyOption.setSelected(false);   
    fairnessOption=new JCheckBox("Fairness path");
    fairnessOption.setSelected(false);
    reorderingOption=new JCheckBox("Dynamic reordering");
    reorderingOption.setSelected(false);
    modeOption=new JCheckBox("Direct mode");
    modeOption.setSelected(false);
    whyOption.setBackground(Utils.backgroundColor);
    fairnessOption.setBackground(Utils.backgroundColor);
    reorderingOption.setBackground(Utils.backgroundColor);
    modeOption.setBackground(Utils.backgroundColor);
    
    commandsPanel.add(whyOption);
    commandsPanel.add(fairnessOption);
    commandsPanel.add(reorderingOption);
    commandsPanel.add(modeOption);
    
   /* layout.putConstraint(SpringLayout.WEST, nameLabel, 10,SpringLayout.WEST, contents);
	  layout.putConstraint(SpringLayout.NORTH, nameLabel, 30, SpringLayout.NORTH, contents);
	  layout.putConstraint(SpringLayout.WEST, nameText, 10,SpringLayout.EAST, nameLabel);
	  layout.putConstraint(SpringLayout.NORTH, nameText, 30,SpringLayout.NORTH, contents);
	  layout.putConstraint(SpringLayout.WEST, why,10,SpringLayout.WEST, contents);
	  layout.putConstraint(SpringLayout.NORTH, why, 80,SpringLayout.NORTH, contents);
	  layout.putConstraint(SpringLayout.WEST, fairness, 10,SpringLayout.WEST, contents);
	  layout.putConstraint(SpringLayout.NORTH, fairness, 110,SpringLayout.NORTH, contents);
	  layout.putConstraint(SpringLayout.NORTH, reordering, 80,SpringLayout.NORTH, contents);
	  layout.putConstraint(SpringLayout.NORTH, mode, 110,SpringLayout.NORTH, contents);	    			    
    layout.putConstraint(SpringLayout.WEST, reordering, 60, SpringLayout.EAST, why);
    Spring con=layout.getConstraint(SpringLayout.WEST, reordering);
    layout.putConstraint(SpringLayout.WEST, mode, con, SpringLayout.WEST, panel);*/
    
	Component[] comps=panel.getComponents();
	
	if(comps.length>0 && !(comps[0] instanceof JButton)){
		
		 int size=comps.length/3;
		 panel.add(b1);
		 panel.add(b02);
		 panel.add(b2);
		 panel.add(bi);
		 panel.add(b5);
		 panel.add(b6);
		 panel.add(b7);		
		 
		 commandsPanel.add(b3);
		 commandsPanel.add(b4);
		 
		
		 layout.putConstraint(SpringLayout.NORTH, b1, BiochamModel.TOP_OFF+BiochamModel.HEIGHT*(size+3),SpringLayout.NORTH, panel);
		 layout.putConstraint(SpringLayout.NORTH, b2, BiochamModel.TOP_OFF+BiochamModel.HEIGHT*(size+3),SpringLayout.NORTH, panel);
		 layout.putConstraint(SpringLayout.NORTH, b02, BiochamModel.TOP_OFF+BiochamModel.HEIGHT*(size+3),SpringLayout.NORTH, panel);
		 layout.putConstraint(SpringLayout.NORTH, bi, BiochamModel.TOP_OFF+BiochamModel.HEIGHT*(size+3),SpringLayout.NORTH, panel);
		 Spring con=layout.getConstraint(SpringLayout.WEST, panel.getComponent(3*(size-1)+1));
		 layout.putConstraint(SpringLayout.WEST, b1, con, SpringLayout.WEST, panel);
		 layout.putConstraint(SpringLayout.WEST, b02, LEFT_OFF, SpringLayout.EAST, b1);
         layout.putConstraint(SpringLayout.WEST, b2, LEFT_OFF, SpringLayout.EAST, b02);
         layout.putConstraint(SpringLayout.WEST, bi, LEFT_OFF+10, SpringLayout.EAST, b2);         
         
    	 layout.putConstraint(SpringLayout.WEST, b5, con, SpringLayout.WEST, panel);
    	 layout.putConstraint(SpringLayout.WEST, b6, LEFT_OFF, SpringLayout.EAST, b5);
    	 layout.putConstraint(SpringLayout.WEST, b7, LEFT_OFF, SpringLayout.EAST, b6);
    	 
         layout.putConstraint(SpringLayout.NORTH, b5, 40,SpringLayout.NORTH, b1);
         layout.putConstraint(SpringLayout.NORTH, b6, 40,SpringLayout.NORTH, b1);
         layout.putConstraint(SpringLayout.NORTH, b7, 40,SpringLayout.NORTH, b1);
         
         commandsPanelLayout.putConstraint(SpringLayout.WEST, b3, LEFT_OFF+10, SpringLayout.WEST, commandsPanel);
         commandsPanelLayout.putConstraint(SpringLayout.WEST, b4, LEFT_OFF+10, SpringLayout.WEST, commandsPanel);
         
         commandsPanelLayout.putConstraint(SpringLayout.NORTH, b3, 10,SpringLayout.NORTH, commandsPanel);
         commandsPanelLayout.putConstraint(SpringLayout.NORTH, b4, 20,SpringLayout.SOUTH, b3);
         
         commandsPanelLayout.putConstraint(SpringLayout.WEST, whyOption,LEFT_OFF+10,SpringLayout.WEST, commandsPanel);
         commandsPanelLayout.putConstraint(SpringLayout.WEST, fairnessOption,LEFT_OFF+10,SpringLayout.WEST, commandsPanel);
         commandsPanelLayout.putConstraint(SpringLayout.WEST, reorderingOption,LEFT_OFF+10,SpringLayout.WEST, commandsPanel);
         commandsPanelLayout.putConstraint(SpringLayout.WEST, modeOption,LEFT_OFF+10,SpringLayout.WEST, commandsPanel);                 
         commandsPanelLayout.putConstraint(SpringLayout.NORTH, whyOption, 40,SpringLayout.SOUTH, b4);
         commandsPanelLayout.putConstraint(SpringLayout.NORTH, fairnessOption, 20,SpringLayout.SOUTH, whyOption);
         commandsPanelLayout.putConstraint(SpringLayout.NORTH, reorderingOption, 20,SpringLayout.SOUTH, fairnessOption);
         commandsPanelLayout.putConstraint(SpringLayout.NORTH, modeOption, 20,SpringLayout.SOUTH, reorderingOption);
              
      
	}else{
		
		 panel.add(b1);
		 panel.add(b02);
		 panel.add(b2);
		 panel.add(bi);			
		 
		 commandsPanel.add(b4);
		 commandsPanelLayout.putConstraint(SpringLayout.WEST, b4, LEFT_OFF+10, SpringLayout.WEST, commandsPanel);		 
		 commandsPanelLayout.putConstraint(SpringLayout.NORTH, b4, BiochamModel.TOP_OFF,SpringLayout.NORTH, commandsPanel);
		 
		 commandsPanelLayout.putConstraint(SpringLayout.WEST, whyOption,LEFT_OFF+10,SpringLayout.WEST, commandsPanel);
         commandsPanelLayout.putConstraint(SpringLayout.WEST, fairnessOption,LEFT_OFF+10,SpringLayout.WEST, commandsPanel);
         commandsPanelLayout.putConstraint(SpringLayout.WEST, reorderingOption,LEFT_OFF+10,SpringLayout.WEST, commandsPanel);
         commandsPanelLayout.putConstraint(SpringLayout.WEST, modeOption,LEFT_OFF+10,SpringLayout.WEST, commandsPanel);                 
         commandsPanelLayout.putConstraint(SpringLayout.NORTH, whyOption, 40,SpringLayout.SOUTH, b4);
         commandsPanelLayout.putConstraint(SpringLayout.NORTH, fairnessOption, 20,SpringLayout.SOUTH, whyOption);
         commandsPanelLayout.putConstraint(SpringLayout.NORTH, reorderingOption, 20,SpringLayout.SOUTH, fairnessOption);
         commandsPanelLayout.putConstraint(SpringLayout.NORTH, modeOption, 20,SpringLayout.SOUTH, reorderingOption);
		 		
		 layout.putConstraint(SpringLayout.NORTH, b1, HEIGHT,SpringLayout.NORTH, panel);
		 layout.putConstraint(SpringLayout.NORTH, b02, HEIGHT,SpringLayout.NORTH, panel);
		 layout.putConstraint(SpringLayout.NORTH, b2, HEIGHT,SpringLayout.NORTH, panel);
		 layout.putConstraint(SpringLayout.NORTH, bi, HEIGHT,SpringLayout.NORTH, panel);
		 layout.putConstraint(SpringLayout.WEST, b1, 50, SpringLayout.WEST, panel);	
		 layout.putConstraint(SpringLayout.WEST, b02, LEFT_OFF+10, SpringLayout.EAST, b1);
		 layout.putConstraint(SpringLayout.WEST, b2, LEFT_OFF+10, SpringLayout.EAST, b02);
		 layout.putConstraint(SpringLayout.WEST, bi, LEFT_OFF+10, SpringLayout.EAST, b2);
		 
		 
	}	
	//commandsPanel.revalidate();
    cs.clear();
    panel.revalidate();
	//panel.repaint();
	
	return panel;
}


public JPanel getCTLModelChecking() {	
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
	
	getSpecifications().clear();
	setSpecifications(null);
	biocham=null;
	panel=null;
	model=null;
	element=null;
	savedResponses=0;
	check.clear();
	check=null;
	
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


public ArrayList<String> getCheck() {
	return check;
}


public boolean isForChecking() {
	return forChecking;
}


public void setForChecking(boolean forChecking) {
	this.forChecking = forChecking;
}


public JTextArea getTarea() {
	return tarea;
}


public void setTarea(JTextArea tarea) {
	this.tarea = tarea;
}




public void actionPerformed(ActionEvent e) {
	if(e.getSource() instanceof CustomToolTipButton){
		CustomToolTipButton b=(CustomToolTipButton)e.getSource();
		b.setBalloonToolTipVisible(false);
	}
	if(e.getActionCommand()=="addCTL"){
		
		revising=false;
		addParameter();
		SwingWorker sw=new SwingWorker(){

			@Override
			public Object construct() {
				//biocham.tree.refreshMenuBar(panel.getName());
				return null;
			}

			@Override
			public void finished() {
				// TODO Auto-generated method stub
				
			}};
			sw.start();
	}
	if(e.getActionCommand()=="reviseModel"){
		
		revising=true;
		tarea.append("\n\n");
		reviseModel();
	}	

	if(e.getActionCommand().equals("saveCTLModelChecking")){
	
		revising=false;
		saveCTLModelChecking();
	}
	
	if(e.getActionCommand()=="checkCTL"){
		
		revising=false;
		tarea.append("\n\n");
		checkCTL();
	}
	
	if(e.getActionCommand()=="nusmv"){
		
		revising=false;
		tarea.append("\n\n");
		nusmv();
	}
	
	if(e.getActionCommand()=="reduceModel"){
		
		revising=false;
		tarea.append("\n\n");
		reduceModel();
	}
	
	if(e.getActionCommand()=="learnRules"){
		
		revising=false;
		tarea.append("\n\n");
		learnRules();
	
	}else if(e.getActionCommand().equals("deleteParameters")){
		model.sendToBiocham("clear_spec.\n");
		this.getSpecifications().clear();		
		ArrayList cs=new ArrayList(0);
		panel.removeAll();			
		panel=refreshPanel(cs);			
		cs.clear();
		cs=null;
		int numComps1=panel.getComponentCount();
		int dynHeight1=(numComps1/2)*40;					
		panel.setPreferredSize(new Dimension(panel.getSize().width,dynHeight1));
		panel.revalidate();
		
	
	}else if(e.getActionCommand()=="addGenCTL"){
	
		
		SwingWorker sw1=new SwingWorker(){
			
			@Override
			public Object construct() {
				//model.createModelBackup();
				setNotToAdd(false);
				setSavedResponses(-1);
				String s="add_genCTL.\n";	
				setForChecking(true);
				setAddingGenerated(true);
				checkAddingGenerated();
				model.sendToBiocham(s,"CTL");
				return null;
			}

			@Override
			public void finished() {
				// refreshCTLSpecifications();	 
				
			}
		};
		sw1.start();
	
	}else if(e.getActionCommand()=="genCTL"){
		
		SwingWorker sw=new SwingWorker(){

			@Override
			public Object construct() {    					
				String s="genCTL.\n";    					
				model.sendToBiocham(s,"CTL");
				return null;
			}
			@Override
			public void finished() {}
		};
		sw.start();
	}
	
}


public void addParameter() {
	setNewAdded(true);
	setNotToAdd(false);
	setSavedResponses(-1);
	element=model.getCtlSpecifications();
	DialogAddSpecification params=new DialogAddSpecification(BiochamMainFrame.frame, element, "CTL Operators",panel);
	
	
	String value=params.getFormula();
	if(value!=null && value!=""){
		
		int i=indexOf(value);
		if(i<0){    					
			String s="add_spec("+value+").\n";	
			setForChecking(true);
			model.sendToBiocham(s,"CTL");
		}else{
			JOptionPane.showMessageDialog(BiochamMainFrame.frame,"The specification  "+value+"  is already definied.");
		}
	}
}


public void reviseModel() {
	
	BiochamModelElement el=model.getModelElement("Boolean Temporal Properties");
	//el.getModel().createModelBackup();
	DialogOptions od=new DialogOptions(BiochamMainFrame.frame,el,"Revise Model",panel);
	String cmd="";
	Object[] newParam=new Object[2];
	newParam=od.getNewParameter();
	Object[] name=(Object[]) newParam[0];			
	String sec=(String)newParam[1];
	od.disposeObject();
	od=null;				
	newParam=null;
	if(name!=null && sec!=null){
		
		if(sec.equals("nothing")){
		
			if(name.length==0){
				cmd="revise_model.\n";
				setForChecking(true);
				if(cmd!=null){
					model.sendToBiocham(cmd,"CTL");
				}
			}else{
				String s="{"+name[0];
				for(int i=1;i<name.length;i++){
					s+=","+name[i];
				}
				s+="}";
				cmd="revise_model("+s+").\n";
				setForChecking(true);
				if(cmd!=null){
					model.sendToBiocham(cmd,"CTL");
				}
			}					
		}else if(sec.equals("selected")){
			if(name.length==0){
				cmd="revise_model_interactive.\n";
				setForChecking(true);
				if(cmd!=null){
					model.sendToBiocham(cmd,"CTL");
				}
			}else{
				String s="{"+name[0];
				for(int i=1;i<name.length;i++){
					s+=","+name[i];
				}
				s+="}";
				cmd="revise_model_interactive("+s+").\n";
				setForChecking(true);
				if(cmd!=null){
					model.sendToBiocham(cmd,"CTL");
				}
			}		
		}
		tarea.append(" Model revising started.........\n");
	}
}


public void saveCTLModelChecking() {
	final String text=tarea.getText();
	Utils.fileChooser.setDialogTitle("Save file as");
	Utils.fileChooser.setDialogType(JFileChooser.SAVE_DIALOG);
	Utils.fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
	String rep=Utils.showSaveDialog("",BiochamMainFrame.frame,SupportedSuffixes.TEXT_SUFFIX);			
	if (rep!=null) {
          
	   final String dir=( new File(rep)).getAbsolutePath();
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
			
		}};
		sw.start();
	}
}


public void checkCTL() {
	
	String[] possibilities = {"check", "check + why", "check all"};  
	
	String msg="<html>How do you want to test the adequacy of the model w.r.t. its specification?<br><br>"+
				"<b><u>Check :</u></b> For each unsatisfied CTL property computes the result of why.<br>"+
				"<b><u>Check + why :</u></b>   For each CTL property computes the result of why.<br>"+
				"<b><u>Check all :</u></b>          Summarizes the result with the first unsatisfied property if there is one.</html>";
	CustomInputDialog d=new CustomInputDialog(view.getParentFrame(),"Check Model's CTL Specification", msg, false, "", "",possibilities);
	if(d.getInputVal()!=null && d.getInputVal().length()>0){
		   String cmd="";
		   boolean reordering=((ParamTableCTLSpecifications)model.getCtlSpecifications().getParamTable()).getReorderingOption().isSelected();
		   boolean fairness=((ParamTableCTLSpecifications)model.getCtlSpecifications().getParamTable()).getFairnessOption().isSelected();
		   boolean mode=((ParamTableCTLSpecifications)model.getCtlSpecifications().getParamTable()).getModeOption().isSelected();
		   //boolean why=((ParamTableCTLSpecifications)model.getCtlSpecifications().getParamTable()).getWhyOption().isSelected();
		   if(reordering){
			   cmd+="nusmv_dynamic_reordering.\n";
		   }else{
			   cmd+="nusmv_disable_dynamic_reordering.\n";
		   } 
		   if(fairness){
			   cmd+="fairness_path.\n";
		   }else{
			   cmd+="no_fairness_path.\n";
		   } 
		   if(mode){
			   cmd+="nusmv_direct.\n";
		   }else{
			   cmd+="nusmv_non_direct.\n";
		   }
		   /*if(why){
			   cmd+="check_why";
		   }else {
			   cmd+="check_ctl";
		   }*/
		   
		if(d.getInputVal().equals("check")){
			cmd+="check_spec.\n";
		}else if(d.getInputVal().equals("check + why")){
			cmd+="check_why_spec.\n";
		}else{
			cmd+="check_all_spec.\n";
		}
		
		setForChecking(true);
		if(cmd!=null){
			tarea.append(" Checking Model's CTL Specifications.........\n");
			model.sendToBiocham(cmd,"CTL");
		}
	}
	/*Object[] possibilities = {"check", "check + why", "check all"};  
	
	String s = (String)JOptionPane.showInputDialog(BiochamMainFrame.frame,
				" \nHow do you want to test the adequacy of the model w.r.t. its specification?\n\n"+
				"Check :              For each unsatisfied CTL property computes the result of why.\n"+
				"Check + why :   For each CTL property computes the result of why.\n"+
				"Check all :          Summarizes the result with the first unsatisfied property if there is one."+"\n\n\n",
				"Check Model's CTL Specification",
				JOptionPane.PLAIN_MESSAGE,
				Icons.icons.get("Ok-32x32.png"),
				possibilities,
				"check");
	
	String cmd="";
	if ((s != null) && (s.length() > 0)) {
		
		   boolean reordering=((ParamTableCTLSpecifications)model.getCtlSpecifications().getParamTable()).getReorderingOption().isSelected();
		   boolean fairness=((ParamTableCTLSpecifications)model.getCtlSpecifications().getParamTable()).getFairnessOption().isSelected();
		   boolean mode=((ParamTableCTLSpecifications)model.getCtlSpecifications().getParamTable()).getModeOption().isSelected();
		//   boolean why=((ParamTableCTLSpecifications)model.getCtlSpecifications().getParamTable()).getWhyOption().isSelected();
		   if(reordering){
			   cmd+="nusmv_dynamic_reordering.\n";
		   }else{
			   cmd+="nusmv_disable_dynamic_reordering.\n";
		   } 
		   if(fairness){
			   cmd+="fairness_path.\n";
		   }else{
			   cmd+="no_fairness_path.\n";
		   } 
		   if(mode){
			   cmd+="nusmv_direct.\n";
		   }else{
			   cmd+="nusmv_non_direct.\n";
		   }
		   if(why){
			   cmd+="check_why";
		   }else {
			   cmd+="check_ctl";
		   }
		   
		if(s.equals("check")){
			cmd+="check_spec.\n";
		}else if(s.equals("check + why")){
			cmd+="check_why_spec.\n";
		}else{
			cmd+="check_all_spec.\n";
		}
		setForChecking(true);
		if(cmd!=null){
			tarea.append(" Checking Model's CTL Specifications.........\n");
			model.sendToBiocham(cmd,"CTL");
		}
	}*/
}


public void nusmv() {
	
	BiochamModelElement el=model.getModelElement("Boolean Temporal Properties");
	//el.getModel().createModelBackup();
	DialogOptions od=new DialogOptions(BiochamMainFrame.frame,el,"Check CTL Property against the model",panel);
	String cmd="";
	Object[] newParam=new Object[2];
	newParam=od.getNewParameter();
	String name=(String)newParam[0];
	ArrayList<String> values=new ArrayList<String>();
	values=(ArrayList<String>)newParam[1];	
	if(name!=null && name.length()>0){					
		cmd=values.get(0)+values.get(1)+values.get(2)+values.get(3)+"("+name+").\n";
		setForChecking(true);
		if(cmd!=null){
			tarea.append(" Model Checker NUSMV started............\n");
			model.sendToBiocham(cmd,"CTL");
		}
		values.clear();
		values=null;
	}else if(name==""){
		JOptionPane.showMessageDialog(BiochamMainFrame.frame, "You didn't specify a biocham query.");
	}
	od.disposeObject();
	od=null;				
	newParam=null;
}


public void reduceModel() {
	BiochamModelElement el=model.getModelElement("Boolean Temporal Properties");
	//el.getModel().createModelBackup();
	DialogOptions od=new DialogOptions(BiochamMainFrame.frame,el,"Reduce Model",panel);
	String cmd="";
	Object[] newParam=new Object[2];
	newParam=od.getNewParameter();
	Object[] name=(Object[]) newParam[0];			
	String sec=(String)newParam[1];
	od.disposeObject();
	od=null;				
	newParam=null;
	if(name!=null && sec!=null){					
		if(name.length==0){
			cmd="reduce_model.\n";
			setForChecking(true);
			if(cmd!=null){	
				model.sendToBiocham(cmd,"CTL");
			}
		}else{
			String s="{"+name[0];
			for(int i=1;i<name.length;i++){
				s+=","+name[i];
			}
			s+="}";
			cmd="reduce_model("+s+").\n";
			setForChecking(true);
			if(cmd!=null){
				model.sendToBiocham(cmd,"CTL");
			}
		}
		tarea.append(" Reducing model started.........\n");
	}
}


public void learnRules() {
	BiochamModelElement el=model.getModelElement("Boolean Temporal Properties");
	//el.getModel().createModelBackup();
	DialogOptions od=new DialogOptions(BiochamMainFrame.frame,el,"Learn Rules",panel);
	String cmd="";
	Object[] newParam=new Object[2];
	newParam=od.getNewParameter();		
	String choice=(String)newParam[1];
	if(choice!=null){
		if(choice.equals("addition")){
			
			String rule=(String) newParam[0];
			if(rule!=null && rule.length()>0){
				String s="{"+rule+"}";
				cmd="learn_one_addition("+s+").\n";
				setForChecking(true);
				if(cmd!=null){
					tarea.append(" Learning rules started.........\n");
					model.sendToBiocham(cmd,"CTL");
				}
			}else{
				JOptionPane.showMessageDialog(BiochamMainFrame.frame, "You must specify a rule to be added to the model.");
			}
		}else{
			
			Object[] name=(Object[]) newParam[0];
			if(name!=null){
				if(name.length==0){
					cmd="learn_one_deletion.\n";
				}else{
					String s="{"+name[0];
					for(int i=1;i<name.length;i++){
						s+=","+name[i];
					}
					s+="}";
					cmd="learn_one_deletion("+s+").\n";
				}
				setForChecking(true);
				if(cmd!=null){
					tarea.append(" Learning rules started.........\n");
					model.sendToBiocham(cmd,"CTL");
				}
			}
		}
	}
	od.disposeObject();
	od=null;				
	newParam=null;
}


	public void refreshCTLSpecifications() {
		
		ArrayList<Component> cs=new ArrayList<Component>();
		Component[] comps=panel.getComponents();
		for(int i=0;i<comps.length;i++){
			if((comps[i] instanceof JButton)){
				if(!((JButton)comps[i]).getActionCommand().equals("addCTL") && !((JButton)comps[i]).getActionCommand().equals("addGenCTL")
						&& !((JButton)comps[i]).getActionCommand().equals("checkCTL") && !((JButton)comps[i]).getActionCommand().equals("genCTL") && !((JButton)comps[i]).getActionCommand().equals("nusmv")
						&& !((JButton)comps[i]).getActionCommand().equals("reduceModel")&& !((JButton)comps[i]).getActionCommand().equals("learnRules") 
						&& !((JButton)comps[i]).getActionCommand().equals("reviseModel")){
					
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
		int dynHeight1=(numComps1/2)*40;					
		panel.setPreferredSize(new Dimension(panel.getSize().width,dynHeight1));
		panel.revalidate();
		
	}
public boolean isRevising() {
	return revising;
}


public void setRevising(boolean revising) {
	this.revising = revising;
}


public boolean isNewAdded() {
	return newAdded;
}


public void setNewAdded(boolean newAdded) {
	this.newAdded = newAdded;
}


public boolean isAddingGenerated() {
	return addingGenerated;
}


public void setAddingGenerated(boolean addingGenerated) {
	this.addingGenerated = addingGenerated;
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


public JPanel getCommandsPanel() {
	return commandsPanel;
}


public void setCommandsPanel(JPanel commandsPanel) {
	this.commandsPanel = commandsPanel;
}


public JCheckBox getFairnessOption() {
	return fairnessOption;
}


public void setFairnessOption(JCheckBox fairnessOption) {
	this.fairnessOption = fairnessOption;
}


public JCheckBox getModeOption() {
	return modeOption;
}


public void setModeOption(JCheckBox modeOption) {
	this.modeOption = modeOption;
}


public JCheckBox getReorderingOption() {
	return reorderingOption;
}


public void setReorderingOption(JCheckBox reorderingOption) {
	this.reorderingOption = reorderingOption;
}


public JCheckBox getWhyOption() {
	return whyOption;
}


public void setWhyOption(JCheckBox whyOption) {
	this.whyOption = whyOption;
}
public void setSpecifications(Vector<CTLSpecification> specifications) {
	this.specifications = specifications;
}
public Vector<CTLSpecification> getSpecifications() {
	return specifications;
}
public CTLView getView() {
	return view;
}
public void setView(CTLView view) {
	this.view = view;
}













}
