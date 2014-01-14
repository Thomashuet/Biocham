package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.BiochamModelElement;
import fr.inria.contraintes.biocham.WorkbenchArea;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.customComponents.DeleteButton;
import fr.inria.contraintes.biocham.dialogs.CustomInputDialog;
import fr.inria.contraintes.biocham.dialogs.DialogAddSpecification;
import fr.inria.contraintes.biocham.menus.BiochamMenuBar;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.SupportedSuffixes;
import fr.inria.contraintes.biocham.utils.Utils;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.util.ArrayList;
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


public class ParamTableConservationLaws implements Parameters, ActionListener{

	
	
	Vector<ConservationLaws> conservationLaws;
	WorkbenchArea biocham;
	JPanel panel;
	BiochamModel model;
	BiochamModelElement element;
	int savedResponses;
	boolean deleting=false;
	ArrayList<String> origValues,modified;
	boolean notToAdd=false;
	JPanel checkingPanel, buttonPanel;
	JTextArea tarea;
	private boolean forChecking=true;
	boolean newAdded=false;
	//private ObserverString observer;
	InvariantsModel invariantsModel;
	InvariantsView view;
	
	public InvariantsView getView() {
		return view;
	}
	public void setView(InvariantsView view) {
		this.view = view;
	}
	public InvariantsModel getInvariantsModel() {
		return invariantsModel;
	}
	public void setInvariantsModel(InvariantsModel invariantsModel) {
		this.invariantsModel = invariantsModel;
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
	
	
	public ParamTableConservationLaws(BiochamModel m,WorkbenchArea workbench, JPanel p){
		
		biocham=workbench;
		model=m;
		panel=p;
		conservationLaws=new Vector<ConservationLaws>();
		savedResponses=-1;
		origValues=new ArrayList<String>();
		modified=new ArrayList<String>();
		
		checkingPanel=new JPanel();			 
		checkingPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createLineBorder(Color.black),"Conservation Laws Checking"));
		checkingPanel.setLayout(new BorderLayout());	
		checkingPanel.setBackground(Utils.backgroundColor);
		checkingPanel.setName("Conservation Laws Checking");
		tarea=new JTextArea();
		tarea.setBackground(Utils.backgroundColor);
		tarea.setEditable(false);
		checkingPanel.add(tarea,BorderLayout.CENTER);
		JButton clear=new JButton("Clear Screen");
		clear.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
				tarea.setText("");
				
			}});
		JButton save=new JButton("Save");
		save.setActionCommand("saveConservationLawsChecking");
		save.addActionListener(this);
		buttonPanel=new JPanel();
		buttonPanel.setLayout(new FlowLayout());
		buttonPanel.setBackground(Utils.backgroundColor);
		buttonPanel.add(clear);
		buttonPanel.add(save);
		checkingPanel.add(buttonPanel,BorderLayout.LINE_END);
		
		invariantsModel=new InvariantsModel(model);		
		view=new InvariantsView(BiochamMainFrame.frame,invariantsModel);
	}
	
	
	
	
	public class ConservationLaws {
	    
		
	private String value,origValue;
		
	ConservationLaws(String v){			
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
	
	
	
	
	
	public int size() {
		return conservationLaws.size();
	}

	public void setModified(Component comp) {
		comp.setBackground(Utils.modifiedParameterColor);	
		
	}
	
	public void disposeElements() {
		conservationLaws.clear();
		conservationLaws=null;
		biocham=null;
		panel=null;
		model=null;
		element=null;
		savedResponses=0;
		
		
	}

	public String getName(int i) {
		return conservationLaws.get(i).getValue();
	}

	public String getValue(int i) {
		return conservationLaws.get(i).getValue();
	}

	public int indexOf(String paramName) {
		 int i=0;
	     while (i<conservationLaws.size() && !getName(i).equals(paramName))
	         i++;
	     if (i == conservationLaws.size())
	         return -1;
	     return i;
	}

	
	
	public void setValue(ArrayList<String> list) {
		
		
		String arg1;
		arg1=(String)list.get(0);	
		int i=indexOf(arg1);		
		JFormattedTextField tf=new JFormattedTextField();
		invariantsModel.addInvariant(arg1);
		if (i<0) {
			
			Component[] comps=panel.getComponents();
	        
			for(int k=0;k<comps.length;k++){
			
				if(comps[k].getName().contains("Add")){
	        		panel.remove(k);	       		 	
	        	}
			}		
			conservationLaws.add(new ConservationLaws(arg1));		
		
			BiochamMenuBar.refreshMenusContext(model);
			int numParam = conservationLaws.size()-1;		
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
			tf.setHorizontalAlignment(JTextField.LEFT);
			tf.setEditable(false);			
	    	tf.setBackground(Color.white);
	    	
	    	DeleteButton but2=new DeleteButton();
	    	but2.setName(arg1);
	    	but2.addMouseListener(new MouseAdapter(){

	    		public void mouseClicked(MouseEvent e){
	    			Component button=(Component) e.getSource();
    				String name=button.getName();
    				int k=indexOf(name);
    				conservationLaws.remove(k);
    				BiochamMenuBar.refreshMenusContext(model);	    				
    				getOrigValues().remove(k);
    				setNotToAdd(true);
    				String s="delete_conservation("+name+").\n";    				
					model.sendToBiocham(s,"conservationLaws");		
    			    Component[] comps=panel.getComponents();
    			    ArrayList cs=new ArrayList();   			    
    			    for(int i=0;i<comps.length;i++){
    			    	if(comps[i].getName().equals(name)){
    			    		cs.add(comps[i]);
    			    	}
    			    }	    			      			   
    			    JFormattedTextField tf= (JFormattedTextField) cs.get(0);
    			    JLabel but1=(JLabel)cs.get(1);
    			    panel.remove(tf);
    			    panel.remove(but1);
    			    comps=null;
    			    comps=panel.getComponents();
    			    cs.clear();
    			    for(int i=0;i<comps.length;i++){	
    			    	if((comps[i] instanceof JButton)){
							if(!((JButton)comps[i]).getActionCommand().contains("add")){
								cs.add(comps[i]);
							}
						}else if((comps[i] instanceof JLabel)){
							if(!((JLabel)comps[i]).getName().equals("refresh")){
								cs.add(comps[i]);
							}
						}else{
							cs.add(comps[i]);
						}		    			    		
    			    }
    			    /*for(int i=0;i<comps.length;i++){	
    			    	if((comps[i] instanceof JLabel)){
							if(((JLabel)comps[i]).getToolTipText().equals("delete") ){
								cs.add(comps[i]);
							}
						}else if((comps[i] instanceof JLabel)){
							if(!((JLabel)comps[i]).getName().equals("refresh")){
								cs.add(comps[i]);
							}
						}else if(!(comps[i] instanceof JButton)){
							cs.add(comps[i]);
						}
    			    		
    			    }*/
    			    panel.removeAll();
    			    refreshPanel(cs);	    			    
    			    cs.clear();	    		
    			    panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));	
    			    panel.getParent().validate();
    				panel.getParent().repaint();
    				cs=null;
    			    comps=null;
    			    button=null;	    			   
    			    tf=null;
	    		}
	    	});
	    		    	    	
	    	panel.add(tf);	  
	    	panel.add(but2); 
	     
	    	layout.putConstraint(SpringLayout.WEST, tf, LEFT_OFF,SpringLayout.WEST, panel);
	    	layout.putConstraint(SpringLayout.NORTH, tf, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);
	    	layout.putConstraint(SpringLayout.WEST, but2, 5, SpringLayout.EAST, tf);	     
	    	layout.putConstraint(SpringLayout.NORTH, but2, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);  
	     
	    	int max=10,temp=0;
	    	for(int j=0;j<conservationLaws.size();j++){
	    		if(panel.getComponent(2*j) instanceof JFormattedTextField){
	    			temp=((JFormattedTextField)panel.getComponent(2*j)).getColumns();
	    			if(temp>=max){
	    				max=temp;
	    			}
	    		}
	    	}
	    	for(int j=0;j<conservationLaws.size();j++){
	    		if(panel.getComponent(2*j) instanceof JFormattedTextField){
	    			((JFormattedTextField)panel.getComponent(2*j)).setColumns(max);//.setColumns(max+1);	    
	    		}
	    	}			
	    	for (i=0;i<conservationLaws.size();++i)
	    		maxSpring = Spring.max(maxSpring, Spring.sum(layout.getConstraints(panel.getComponent(2*i)).getWidth(),Spring.constant(10)));
	    	for (i=0;i<conservationLaws.size();++i)
	    		layout.putConstraint(SpringLayout.WEST, panel.getComponent(2*i+1), maxSpring, SpringLayout.WEST, panel);	    	
	    	
	    	
	    	
	    	/*int max=30;
	  		String elem_max = null;
	      	for(int j=0;j<conservationLaws.size();j++){
	      		if(panel.getComponent(2*j) instanceof JFormattedTextField){
	      			elem_max=((JFormattedTextField)panel.getComponent(2*j)).getText();
	      			if(elem_max.length()>=max){
	      				max=elem_max.length();
	      			}
	      		}
	      	}
	      	for(int j=0;j<conservationLaws.size();j++){
	      		if(panel.getComponent(2*j) instanceof JFormattedTextField){
	      			if(max>30){
	      				max+=10;
	      			}
	      			((JFormattedTextField)panel.getComponent(2*j)).setColumns(max-max/2); 
	      		}
	      	}
	          
	    	for (i=0;i<conservationLaws.size();++i)
	    		maxSpring = Spring.max(maxSpring, Spring.sum(layout.getConstraints(panel.getComponent(2*i)).getWidth(),Spring.constant(10)));
	    	for (i=0;i<conservationLaws.size();++i)
	    		layout.putConstraint(SpringLayout.WEST, panel.getComponent(2*i+1), maxSpring, SpringLayout.WEST, panel);	     */
		}else{
			//JOptionPane.showMessageDialog(BiochamMainFrame.frame,"The conservation law >> "+arg1+" << is already definied.");
		}
		
		refreshAfterAddingNew();
		if(isNewAdded()){
			refreshAfterAddingNew();
			setNewAdded(false);
		}
		panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));
		
	}

	synchronized public JPanel refreshPanel(ArrayList cs) {

		Spring maxSpring = Spring.constant(MIDDLE);
		SpringLayout layout = (SpringLayout) panel.getLayout();		
		int rows = cs.size()/2;	
		for(int i=0;i<cs.size();i++){
			if(((Component)cs.get(i)).getName().equals("refresh")){
				cs.remove(i);
			}			
		}
		/*for(int i=0;i<cs.size();i++){
			if(cs.get(i) instanceof JLabel){
				if(!((JLabel)cs.get(i)).getToolTipText().equals("delete")){	
					cs.remove(i);
				}
			}
		}*/
		rows = cs.size()/2;			
		for(int i=0;i<rows;i++){
			 
			JComponent cp=(JComponent) cs.get(2*i);
			
			JFormattedTextField tf=(JFormattedTextField)cp;		
			JLabel but1=(JLabel)cs.get(2*i+1);		
			panel.add(tf);
			panel.add(but1);
			
			layout.putConstraint(SpringLayout.WEST, tf, LEFT_OFF, SpringLayout.WEST, panel);
			layout.putConstraint(SpringLayout.NORTH, tf, TOP_OFF+20+HEIGHT*i,SpringLayout.NORTH, panel);		
			layout.putConstraint(SpringLayout.NORTH, but1, TOP_OFF+20+HEIGHT*i,SpringLayout.NORTH, panel);
			layout.putConstraint(SpringLayout.WEST, but1, 5, SpringLayout.EAST, tf);
			
			
	    	
			/*
			 int max=30;
		  		String elem_max = null;
		  		int cnt=panel.getComponentCount();
		      	for(int j=0;j<conservationLaws.size() && 2*j<cnt;j++){
		      		if(panel.getComponent(2*j) instanceof JFormattedTextField){
		      			elem_max=((JFormattedTextField)panel.getComponent(2*j)).getText();
		      			if(elem_max.length()>=max){
		      				max=elem_max.length();
		      			}
		      		}
		      	}
		      	for(int j=0;j<conservationLaws.size() && 2*j<cnt;j++){
		      		if(panel.getComponent(2*j) instanceof JFormattedTextField){
		      			if(max>30){
		      				max+=10;
		      			}
		      			((JFormattedTextField)panel.getComponent(2*j)).setColumns(max-max/2); 
		      		}
		      	}*/
				   
		}
		
		
		int max=10;
		String elem_max = null;
    	for(int j=0;j<conservationLaws.size();j++){
    		try{
    			if(panel.getComponent(2*j) instanceof JFormattedTextField){
    				elem_max=((JFormattedTextField)panel.getComponent(2*j)).getText();
    				if(elem_max.length()>=max){
    					max=elem_max.length();
    				}
    			}
    		}catch(Exception e){}
    	}
    	for(int j=0;j<conservationLaws.size();j++){
    		try{
    			if(panel.getComponent(2*j) instanceof JFormattedTextField){
    				((JFormattedTextField)panel.getComponent(2*j)).setColumns(max-max/4); 
    			}
    		}catch(Exception e){}
    	}
		
    	Utils.debugMsg("Declarations.size="+conservationLaws.size());
    	for (int i=0;i<conservationLaws.size();++i)
    		maxSpring = Spring.max(maxSpring, Spring.sum(layout.getConstraints(panel.getComponent(2*i)).getWidth(),Spring.constant(10)));
    	for (int i=0;i<conservationLaws.size();++i)
    		layout.putConstraint(SpringLayout.WEST, panel.getComponent(2*i+1), maxSpring, SpringLayout.WEST, panel);
    	    	
    	
		String toolTipText="<html><i>Declares a new mass conservation law for all the molecules matched <br>" +
		 		"by the argument if it is a set, or for all molecules given with the <br> corresponding weight." +
		 		"<br>Example: {cycE-?} or [A-A, 2*A]. </i></html>";
		CustomToolTipButton b1=new CustomToolTipButton("Add",toolTipText);			
		b1.setName("Add");
		b1.setActionCommand("addConservationLaw");
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
	    /*toolTipText="<html><i>Will check all conservation laws against rules, and if necessary kinetics.</i><html>";
	    CustomToolTipButton b2=new CustomToolTipButton("Check",toolTipText);
	    b2.setName("CheckConservationLaw");		
	    b2.setActionCommand("checkConservationLaw");
	    b2.addActionListener(this);
		b2.setBalloonToolTipVisible(false);
		
		toolTipText="<html><i>Computes the P-invariants of the system, and thus conservation laws <br>" +
				"that are independent from the precise kinetics. One can give a limit on the highest value " +
				"<br> found in a given P-invariant (the default is 4), but note that giving a too high value <br>" +
				"might lead to a very long computation.</i></html>";
		CustomToolTipButton b3=new CustomToolTipButton("Search",toolTipText);
	    b3.setName("P-invariants");
	    b3.setActionCommand("P-invariants");
	    b3.addActionListener(this);	
	    b3.setBalloonToolTipVisible(false);*/
		Component[] comps=panel.getComponents();
		
		if(comps.length>0 && !(comps[0] instanceof JButton)){
			
			 
			 panel.add(b1);	
			 panel.add(bi);
			 
			 layout.putConstraint(SpringLayout.NORTH, b1,40,SpringLayout.NORTH,comps[comps.length-1]);
	         layout.putConstraint(SpringLayout.WEST, b1, LEFT_OFF, SpringLayout.WEST, panel);
	         layout.putConstraint(SpringLayout.NORTH, bi, 0,SpringLayout.NORTH, b1);
	         layout.putConstraint(SpringLayout.WEST, bi, 10, SpringLayout.EAST, b1);
				
		}else{
			
			 panel.add(b1);
			 panel.add(bi);
			 
			 layout.putConstraint(SpringLayout.NORTH, b1, HEIGHT,SpringLayout.NORTH, panel);
			 layout.putConstraint(SpringLayout.WEST, b1, 10, SpringLayout.WEST, panel);
			 layout.putConstraint(SpringLayout.NORTH, bi, HEIGHT,SpringLayout.NORTH, panel);
			 layout.putConstraint(SpringLayout.WEST, bi, 10, SpringLayout.EAST, b1);	
			 
		}				
	    cs.clear();
	    panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));	
	    panel.validate();
		panel.repaint();			
		
		 
		return panel;
		
	}


	public void actionPerformed(ActionEvent e) {
		
		if(e.getActionCommand()=="addConservationLaw"){			
			addParameter();
			BiochamMenuBar.refreshMenusContext(model);
			
		}else		
		if(e.getActionCommand().equals("saveConservationLawsChecking")){
		
			saveConservationLawsChecking();
		}else
		
		if(e.getActionCommand()=="checkConservationLaw"){
			if(e.getSource() instanceof CustomToolTipButton){
				CustomToolTipButton b=(CustomToolTipButton)e.getSource();
				b.setBalloonToolTipVisible(false);
			}
			checkConservationLaw();
		}else	
		if(e.getActionCommand()=="P-invariants"){
			if(e.getSource() instanceof CustomToolTipButton){
				CustomToolTipButton b=(CustomToolTipButton)e.getSource();
				b.setBalloonToolTipVisible(false);
			}
			
			String msg="<html>Enter a limit on the highest value<br>"+
			"found in a given P-invariant (the default is 4):</html>";
			CustomInputDialog d=new CustomInputDialog(view.getParentFrame(),"P-Invariant limit", msg, true, "4", "",null);
			if(d.getInputVal()!=null && d.getInputVal().length()>0){
				if(!d.getInputVal().equals("4")){
					try{
						int k=Integer.parseInt(d.getInputVal());
						computePinvariants(k);
					}catch(NumberFormatException ex){
						JOptionPane.showMessageDialog(view.getParentFrame(), "The input should be an Integer value.","Error",JOptionPane.ERROR_MESSAGE);
					}
				}else{
					computePinvariants(4);
				}
			}
		}
		else if(e.getActionCommand().equals("deleteAll")){
			for(int i=0;i<conservationLaws.size();i++){
				model.sendToBiocham("delete_conservation("+conservationLaws.get(i).getValue()+").\n");
			}
			
			conservationLaws.clear();
			BiochamMenuBar.refreshMenusContext(model);
			panel.removeAll();			
			ArrayList cs=new ArrayList(0);
			panel=refreshPanel(cs);			
			cs.clear();
			cs=null;			
			panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));	
			panel.revalidate();
		}
	}
	
	/**
	 * 
	 */
	public void refreshAfterAddingNew() {
		ArrayList<Component> cs=new ArrayList<Component>();
		Component[] comps=panel.getComponents();
		for(int i=0;i<comps.length;i++){
			if((comps[i] instanceof JButton)){
				if(!((JButton)comps[i]).getActionCommand().equals("addConservationLaw") && !((JButton)comps[i]).getActionCommand().equals("checkConservationLaw")){							
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
		panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));	
		panel.revalidate();
		
	}
	
	public void computePinvariants(int i) {
		
		String cmd=null;
		if(i==-1){
			cmd="search_conservations.\n";
		}else{
			cmd="search_conservations("+i+").\n";
		}
		setForChecking(true);	             
        JPanel pan=getCheckingPanel();
        Component[] comps=pan.getComponents();
        int size=comps.length;	
        for(int j=0;j<size;j++){
     	   if(comps[j] instanceof JTextArea){
     		   JTextArea ta=(JTextArea)comps[j];
     		   ta.append("\n The P-invariants of the system are:\n");			            		  	            		  
     	   }
        }
        pan.repaint();
        pan=null;
        comps=null;		
		model.sendToBiocham(cmd,"conservationLaws");
		
	}

	public void checkConservationLaw() {
		
		String cmd="check_conservations.\n";
		setForChecking(true);	
		model.sendToBiocham(cmd,"conservationLaws");
			
	}
	
	public void saveConservationLawsChecking() {
	
		
		String text=tarea.getText();
		Utils.fileChooser.setDialogTitle("Save file as");
		Utils.fileChooser.setDialogType(JFileChooser.SAVE_DIALOG);
		Utils.fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		String rep=Utils.showSaveDialog("",BiochamMainFrame.frame,SupportedSuffixes.TEXT_SUFFIX);			
		if (rep!=null) {
              
		   String dir=(new File(rep)).getAbsolutePath();
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
		}
	}


	public void addParameter() {
		setNewAdded(true);
		setNotToAdd(false);
		setSavedResponses(-1);
		element=model.getConservationLaws();
		DialogAddSpecification params=new DialogAddSpecification(BiochamMainFrame.frame, element, "Conservation Laws",panel);
		
		
		String value=params.getFormula();
		if(value!=null && value!=""){
			
			int i=indexOf(value);
			if(i<0){    					
				String s="conservation("+value+").\n";	
				setForChecking(true);
				model.sendToBiocham(s,"conservationLaws");
			}else{
				JOptionPane.showMessageDialog(BiochamMainFrame.frame,"The conservation law  "+value+"  is already definied.");
			}
		}
		
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
			    panel.getComponent(2*i).setName(ovalue);
			    panel.getComponent(2*i+1).setName(ovalue);
			    tf.setBackground(Color.WHITE);
			    String s1="delete_conservation("+s+").\n";
			    s1+="conservation("+ovalue+").\n";
			    setNotToAdd(true);
				model.sendToBiocham(s1,"conservationLaws");	
				conservationLaws.remove(i);
				conservationLaws.add(i,new ConservationLaws(ovalue));
				refreshAfterAddingNew();
			}
		}else{
			 tf.setBackground(Color.WHITE);
		}
		tf=null;
		
	}

	public void resetSavedResponses() {
		savedResponses=-1;
	}



	

	public WorkbenchArea getBiocham() {
		return biocham;
	}

	public void setBiocham(WorkbenchArea biocham) {
		this.biocham = biocham;
	}

	public Vector<ConservationLaws> getConservationLaws() {
		return conservationLaws;
	}

	public void setConservationLaws(Vector<ConservationLaws> conservationLaws) {
		this.conservationLaws = conservationLaws;
	}

	public boolean isDeleting() {
		return deleting;
	}

	public void setDeleting(boolean deleting) {
		this.deleting = deleting;
	}

	public BiochamModelElement getElement() {
		return element;
	}

	public void setElement(BiochamModelElement element) {
		this.element = element;
	}

	public BiochamModel getModel() {
		return model;
	}

	public void setModel(BiochamModel model) {
		this.model = model;
	}

	public ArrayList<String> getModified() {
		return modified;
	}

	public void setModified(ArrayList<String> modified) {
		this.modified = modified;
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

	public void setOrigValues(ArrayList<String> origValues) {
		this.origValues = origValues;
	}

	public JPanel getPanel() {
		return panel;
	}

	public void setPanel(JPanel panel) {
		this.panel = panel;
	}

	public int getSavedResponses() {
		return savedResponses;
	}

	public void setSavedResponses(int savedResponses) {
		this.savedResponses = savedResponses;
	}

	public JPanel getCheckingPanel() {
		return checkingPanel;
	}

	public void setCheckingPanel(JPanel checkingPanel) {
		this.checkingPanel = checkingPanel;
	}

	public boolean isForChecking() {
		return forChecking;
	}

	public void setForChecking(boolean forChecking) {
		this.forChecking = forChecking;
	}
	public boolean isNewAdded() {
		return newAdded;
	}


	public void setNewAdded(boolean newAdded) {
		this.newAdded = newAdded;
	}

	public JTextArea getTarea() {
		return tarea;
	}
	

}
