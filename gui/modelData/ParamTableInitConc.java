package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.BiochamModelElement;
import fr.inria.contraintes.biocham.WorkbenchArea;
import fr.inria.contraintes.biocham.WorkbenchToolBars;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.customComponents.ModifyButton;
import fr.inria.contraintes.biocham.dialogs.DialogAddParameter;
import fr.inria.contraintes.biocham.graphicalEditor.BiochamEntityData;
import fr.inria.contraintes.biocham.graphicalEditor.GraphUtilities;
import fr.inria.contraintes.biocham.menus.BiochamMenuBar;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.Utils;
import net.java.balloontip.BalloonTip;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.Spring;
import javax.swing.SpringLayout;

public class ParamTableInitConc implements Parameters, ActionListener{
	
	
	
	private Vector<InitialConcentration> initConcentrations;
	WorkbenchArea biocham;
	JPanel panel;
	BiochamModel model;
	BiochamModelElement element;
	int savedResponses;
	boolean notToAdd=false;
	ArrayList<String> origValues;
	BalloonTip bTip=null;	
	boolean newAdded=false;
	boolean modifying=false;
	String lastAdded="";
	boolean dontCheck=false;

	InitialStateModel initStateModel;
	InitialStateView view;
	
	public InitialStateView getView() {
		return view;
	}
	public void setView(InitialStateView view) {
		this.view = view;
	}
	public InitialStateModel getInitStateModel() {
		return initStateModel;
	}
	public void setInitStateModel(InitialStateModel initStateModel) {
		this.initStateModel = initStateModel;
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
	
	public ParamTableInitConc(BiochamModel m,WorkbenchArea workbench, JPanel p){
		
		biocham=workbench;
		model=m;
		panel=p;
		setInitConcentrations(new Vector<InitialConcentration>());
		savedResponses=-1;
		origValues=new ArrayList<String>();
		Utils.modern.setBorderThickness(3);
		Utils.modern.enableAntiAliasing(true);	
		
		initStateModel=new InitialStateModel(model);
		view=new InitialStateView(BiochamMainFrame.frame,initStateModel);
	}
	
	public String getName(int i) {
		return getInitConcentrations().get(i).getName();
	}

	public String getValue(int i) {
		return getInitConcentrations().get(i).getValue();
	}

	public int indexOf(String paramName) {
		int i=0;
	     while (i<getInitConcentrations().size() && !getName(i).equals(paramName))
	         i++;
	     if (i == getInitConcentrations().size())
	         return -1;
	     return i;
	}

	public void resetParameter(String s) {
		
		int i = indexOf(s);
		JFormattedTextField tf = ((JFormattedTextField) panel.getComponent(5*i+1));
		setSavedResponses(-1);
		
		if(!getValue(i).equals(getOrigValues().get(i))){
			
			if (i>=0) {
							
				
				String ovalue=getOrigValues().get(i);				
			    tf.setValue(ovalue);
			    tf.setBackground(Color.WHITE);
			    double val=0;
			    try{
			    	val=Double.parseDouble(ovalue);
			    }catch(Exception e){			    
			    	Utils.debugMsg("The value of "+s+" is not of numerical type...."+val+"?");
			    	WorkbenchToolBars.infoLabel.setText("The value of "+s+" is not of numerical type...."+val+"?");
      			   	WorkbenchToolBars.infoLabel.setBalloonToolTipVisible(true);
			    }
			    String butName="Present";
		    	if(val>0){
		    		butName="Absent";
		    	}
		    	((JButton)panel.getComponent(5*i+3)).setText(butName);
			    String s1="present("+s+","+ovalue+").\n";
			    setNotToAdd(true);
				model.sendToBiocham(s1,"initConc");	
				getInitConcentrations().remove(i);
				getInitConcentrations().add(i,new InitialConcentration(s,ovalue));
				try{
					((BiochamEntityData)GraphUtilities.getCellByName(model.getReactionsGraphEditor(),s).getUserObject()).setInitialConcentration(ovalue);
				}catch(Exception ee){
					ee.printStackTrace();
				}
			}
		}else{
			 tf.setBackground(Color.WHITE);
		}
		tf=null;
		
	}

	public void setModified(Component comp) {
		
		comp.setBackground(Utils.modifiedParameterColor);			
	}
	
	public int size() {
		return getInitConcentrations().size();
	}
	
	
	public void setValue(ArrayList list) {
		
		String arg1,arg2;
		arg1=(String)list.get(0);
		arg2=(String)list.get(1);		
		int i=indexOf(arg1);
		JFormattedTextField tf=new JFormattedTextField();
		tf.setColumns(10);
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
			
			initStateModel.addInitState(arg1, arg2);
			Component[] comps=panel.getComponents();
	        
			for(int k=0;k<comps.length;k++){
				
				if(comps[k].getName().equals("Add")){
	        		panel.remove(k);	       		 	
	        	}
			}
			
			getInitConcentrations().add(new InitialConcentration(arg1,arg2)); //vector of parameters
			
			int numParam = getInitConcentrations().size()-1;
			
			boolean exists=false;
			for(int y=0;y<getOrigValues().size();y++){
				if(numParam==y){
					exists=true;
					break;
				}
			}
			if(!exists){
				getOrigValues().add(numParam,arg2);
			}				

			Spring maxSpring = Spring.constant(MIDDLE);
			SpringLayout layout = (SpringLayout) panel.getLayout();
			JLabel l = new JLabel(arg1);
			l.setName(arg1);  
			panel.add(l);    
			
			tf.setName(arg1);
			l.setLabelFor(tf);
			tf.setValue(arg2);
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
								String name=tf.getName();
								scrC=null;
								
								if(e.getKeyCode()==e.VK_ENTER){
								    
								    int i=indexOf(name);
								    if(i>=0){
								    	
							 			Component[] comps=panel.getComponents();	
										for(int k=0;k<comps.length;k++){
															  
											if(comps[k].getName().equals(name) && comps[k] instanceof JFormattedTextField){
												
												((JFormattedTextField)comps[k]).setValue(tf.getText());
												setModified(((JFormattedTextField)comps[k]));
												JButton button=(JButton)comps[k+2];
												double val=0;
												try{
													String s=tf.getText();
													val=Double.parseDouble(s);
										    		s=null;
													
												}catch(Exception ee){
													 /*System.out.println("The value not of numerical type "+tf.getText());
													 WorkbenchToolBars.infoLabel.setText("The value is not of numerical type"+tf.getText());
													 WorkbenchToolBars.infoLabel.setBalloonToolTipVisible(true);*/
												}
												if(val>0){
													((JButton)button).setText("Absent");																	
												}else{
													((JButton)button).setText("Present");
												}
												((JFormattedTextField)comps[k]).setEditable(false);
												setNotToAdd(true);
												lastAdded=name;
												model.sendToBiocham("present("+name+","+tf.getText()+").\n","initConc");
												getInitConcentrations().remove(i);
												getInitConcentrations().add(i,new InitialConcentration(name,tf.getText()));	
												try{
													((BiochamEntityData)GraphUtilities.getCellByName(model.getReactionsGraphEditor(),name).getUserObject()).setInitialConcentration(tf.getText());
												}catch(Exception ee){
													ee.printStackTrace();
												}
												comps=null;
												tf=null;
												return;
											}
										}
								    }
								    
								}
								
							}
							
						}
						
					}		
					
			   }
			});//(model); //On click ENTER call the action that modifies the value
	    	tf.setBackground(Color.white);
	     
	    	ModifyButton but1=new ModifyButton();	
	    	but1.setName(arg1);
	    	but1.addMouseListener(new MouseAdapter(){

				public void mouseClicked(MouseEvent e) {
					setSavedResponses(-1);
    				Component button=(Component) e.getSource();
    				String name=button.getName();;
    				JPanel panel=(JPanel) button.getParent();//arg1
    			    Component[] comps=panel.getComponents();
    			    Vector cs=new Vector();
    			    for(int i=0;i<comps.length;i++){
    			    	if(comps[i].getName()==name){
    			    		cs.add(comps[i]);			    		
    			    	}
    			    }
    			    JFormattedTextField tf= (JFormattedTextField) cs.get(1);
    			    tf.setEditable(true);
    			    button=null;
    			    comps=null;
    			    panel=null;
    			    cs.clear();
    			    cs=null;
    			    tf=null;
				}
	    	});
	    	
	    	String butName="Present";
	    	double val=0.0;
	    	try{
	    		val=Double.parseDouble(arg2);
	    		
	    	}catch(Exception e){
	    		 /*System.out.println("The value not of numerical type...."+val+"?");
				 WorkbenchToolBars.infoLabel.setText("The value is not of numerical type...."+val+"?");
				 WorkbenchToolBars.infoLabel.setBalloonToolTipVisible(true);*/
	    	}
	    	
	    	if(val>0){
	    		butName="Absent";
	    	}
	    	JButton but2=new JButton(butName);
	    	but2.setActionCommand(butName);
	    	but2.setName(arg1);	    	
	    	but2.addActionListener(new ActionListener(){

				public void actionPerformed(ActionEvent e) {
					setSavedResponses(-1);
					Component[] comps=panel.getComponents();
					JButton button=null;
					for(int k=0;k<comps.length;k++){
						if(comps[k] instanceof JButton && ((JButton)comps[k])==(JButton)e.getSource()){
							button=(JButton)comps[k];
							break;
						}
					}
				
					if(button!=null){
						String name=button.getName();;
	    				JPanel panel=(JPanel) button.getParent();//arg1
	    			    comps=panel.getComponents();
	    			    Vector cs=new Vector();
	    			    for(int i=0;i<comps.length;i++){
	    			    	if(comps[i].getName()==name){
	    			    		cs.add(comps[i]);			    		
	    			    	}
	    			    }
	    			    JFormattedTextField tf= (JFormattedTextField) cs.get(1);
	    			    tf.setEditable(true);
	    			    if(button.getText().contains("Absent")){
	    			    	tf.setText("0.0");
	    			    	((JButton)button).setText("Present");
	    			    }else{
	    			    	tf.setText("1.0");
	    			    	((JButton)button).setText("Absent");
	    			    }
	    			    dontCheck=true;
	    			    String s1="present("+tf.getName()+","+tf.getText()+").\n";
	    			    setNotToAdd(true);
	    			    int ind=indexOf(tf.getName());
	    			    getInitConcentrations().remove(ind);
						getInitConcentrations().add(ind,new InitialConcentration(name,tf.getText()));		
	    				model.sendToBiocham(s1,"initConc");	
	    				bTip.setVisible(false);
	    				tf.setEditable(false);
	    			    button=null;
	    			    comps=null;
	    			    panel=null;
	    			    cs.clear();
	    			    cs=null;
	    			    tf=null;
					}
    				
					
				}});	    
	    	
	    	JButton but3=new JButton("Undefined");
	    	but3.setActionCommand("Undefined");
	    	but3.setName(arg1);	
	    	but3.addActionListener(this);
	    	tf.setColumns(10);
	    	panel.add(tf);
	    	panel.add(but1);
	    	panel.add(but2);
	    	panel.add(but3);
	    	
	    
			layout.putConstraint(SpringLayout.WEST, l, LEFT_OFF, SpringLayout.WEST, panel);
			layout.putConstraint(SpringLayout.NORTH, l, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);		
			layout.putConstraint(SpringLayout.NORTH, but1, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);
	    	layout.putConstraint(SpringLayout.NORTH, but2, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);
	    	layout.putConstraint(SpringLayout.NORTH, but3, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel); 
			layout.putConstraint(SpringLayout.NORTH, tf, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);  
			layout.putConstraint(SpringLayout.WEST, but1, 10, SpringLayout.EAST, tf);	
			layout.putConstraint(SpringLayout.WEST, but2, 5, SpringLayout.EAST, but1);
			layout.putConstraint(SpringLayout.WEST, but3, 5, SpringLayout.EAST, but2);
			
			
	    	for (i=0;i<getInitConcentrations().size();++i)
	    		maxSpring = Spring.max(maxSpring, Spring.sum(layout.getConstraints(panel.getComponent(5*i)).getWidth(),Spring.constant(10)));
	    	for (i=0;i<getInitConcentrations().size();++i)
	    		layout.putConstraint(SpringLayout.WEST, panel.getComponent(5*i+1), maxSpring, SpringLayout.WEST, panel);
	       			
	    	maxSpring=null;
	    	
		}else{
		
	    		setSavedResponses(0);
				Component[] comps=panel.getComponents();	
				for(int k=0;k<comps.length;k++){
									  
					if(comps[k].getName().equals(arg1) && comps[k] instanceof JFormattedTextField){
						
						((JFormattedTextField)comps[k]).setValue(arg2);
						setModified(((JFormattedTextField)comps[k]));
						getInitConcentrations().remove(i);
						getInitConcentrations().add(i,new InitialConcentration(arg1,arg2));
						((JFormattedTextField)comps[k]).setEditable(false);
						comps=null;
						return;
					}
				}		
		}
		refreshAfterAddingNew();
		if(isNewAdded()){
			refreshAfterAddingNew();
			setNewAdded(false);
		}
		panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));
		panel.revalidate();
	}
	
	public void modify(String name,String value){
		
		
		int i=indexOf(name);
		
		if(i>=0){
			
			Component[] comps=panel.getComponents();	
			for(int k=0;k<comps.length;k++){
								  
				if(comps[k].getName().equals(name) && comps[k] instanceof JFormattedTextField){
					
					((JFormattedTextField)comps[k]).setValue(value);
					setModified(((JFormattedTextField)comps[k]));
					((JFormattedTextField)comps[k]).setEditable(false);
					setNotToAdd(true);
					lastAdded=name;
					model.sendToBiocham("present("+name+","+value+").\n","initConc");
					getInitConcentrations().remove(i);
					getInitConcentrations().add(i,new InitialConcentration(name,value));		
					comps=null;				
					return;
				}
			}
			
		}
	}
	
	/**
	    * An inner class that represents the parameter object.
	    *       
	    * @author Sylvain Soliman         
	   */
		public class InitialConcentration {
	      
		  private String name;
	      private String value;

	      InitialConcentration(String s, String k) {
	         name = s;	      
	         value = k;
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
	      public void setValue(String k) {
	          value = new String(k);
	      }

	      public void resetValue() {
	     
	      }

	   }


		synchronized public JPanel refreshPanel(ArrayList cs) {
		
		SpringLayout layout = (SpringLayout) panel.getLayout();
		Spring maxSpring = Spring.constant(MIDDLE);
		
		int rows = cs.size()/5;	
		
		
		for(int i=0;i<rows;i++){
			 
			JComponent cp=(JComponent) cs.get(5*i);
			JLabel l=(JLabel)cp;
			
			JFormattedTextField tf= (JFormattedTextField) cs.get(5*i+1);
			tf.setColumns(10);
			
			JLabel but1=(JLabel)cs.get(5*i+2);
			JButton but2=(JButton)cs.get(5*i+3);
			JButton but3=(JButton)cs.get(5*i+4);
			panel.add(l);
			panel.add(tf);
			panel.add(but1);
			panel.add(but2);
			panel.add(but3);
			
			
			String butName="Present";
	    	double val=0.0;
	    	try{
	    		String s=tf.getText();
	    		val=Double.parseDouble(s);
	    		s=null;
	    	}catch(Exception e){
	    		/* System.out.println("The value not of numerical type...."+val+"?");
				 WorkbenchToolBars.infoLabel.setText("The value is not of numerical type...."+val+"?");
				 WorkbenchToolBars.infoLabel.setBalloonToolTipVisible(true);		*/		
	    	}
	    	if(val>0){
	    		butName="Absent";
	    	}
	    	but2.setText(butName);
	    	
			
			layout.putConstraint(SpringLayout.WEST, l, LEFT_OFF, SpringLayout.WEST, panel);
			layout.putConstraint(SpringLayout.NORTH, l, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);			
			layout.putConstraint(SpringLayout.NORTH, but1, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);
	    	layout.putConstraint(SpringLayout.NORTH, but2, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);
	    	layout.putConstraint(SpringLayout.NORTH, but3, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel); 
			layout.putConstraint(SpringLayout.NORTH, tf, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);  
			layout.putConstraint(SpringLayout.WEST, but1, 10, SpringLayout.EAST, tf);	
			layout.putConstraint(SpringLayout.WEST, but2, 5, SpringLayout.EAST, but1);
			layout.putConstraint(SpringLayout.WEST, but3, 5, SpringLayout.EAST, but2);
		   
		    cp=null;
		    l=null;
		    tf=null;
		    but1=null;
		    
		}
		
		
		
		for (int i=0;i<rows;++i){
	         maxSpring = Spring.max(maxSpring, Spring.sum(layout.getConstraints(panel.getComponent(5*i)).getWidth(),Spring.constant(10)));
	    }
	    for (int i=0;i<rows;++i){
	         layout.putConstraint(SpringLayout.WEST, panel.getComponent(5*i+1), maxSpring, SpringLayout.WEST, panel);
	    }
	    maxSpring=null;
	    
	    String toolTipText="<html><i>Define the initial state of an object.<br>Example: (MA,0.035) or (MA,k1) where k1 is a definied parameter.</i></html>";
	    CustomToolTipButton b1=new CustomToolTipButton("Add",toolTipText);
	    b1.setName("Add");
	    b1.setActionCommand("addInitConc");
	    b1.addActionListener(this);
	    b1.setBalloonToolTipVisible(false);
	    JLabel b2=new JLabel();
	    b2.setIcon(Icons.icons.get("Refresh3.png"));
	    b2.setName("refresh");	    
	    b2.setText("Screen Refresh");
	    b2.setForeground(Utils.refreshedColor);
	    b2.setToolTipText("Click to Refresh the Screen");
	    b2.addMouseListener(new MouseAdapter(){
	    	public void mouseClicked(MouseEvent me) {
	    		
             
                refreshAfterAddingNew();			    	
		    	
            }

			
	    });
		Component[] comps=panel.getComponents();
		
		if(comps.length>0 && !(comps[0] instanceof JButton)){
			
			 panel.add(b1);
			 panel.add(b2);
			 
			 layout.putConstraint(SpringLayout.NORTH, b1,40,SpringLayout.NORTH,comps[comps.length-3]);
	         layout.putConstraint(SpringLayout.WEST, b1, LEFT_OFF, SpringLayout.WEST, panel);
	         layout.putConstraint(SpringLayout.NORTH, b2, 0,SpringLayout.NORTH, b1);
	         layout.putConstraint(SpringLayout.WEST, b2, 10, SpringLayout.EAST, b1);
	        			 
		}else{
			 panel.add(b1);
			 panel.add(b2);
			 
			 layout.putConstraint(SpringLayout.NORTH, b1, HEIGHT,SpringLayout.NORTH, panel);
			 layout.putConstraint(SpringLayout.WEST, b1, 10, SpringLayout.WEST, panel);	
			 layout.putConstraint(SpringLayout.NORTH, b2, HEIGHT,SpringLayout.NORTH, panel);
			 layout.putConstraint(SpringLayout.WEST, b2, 10, SpringLayout.EAST, b1);	
		}		
	    cs.clear();
	    comps=null;
	    layout=null;
	    b1=null;
	    b2=null;
	    panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));
	    panel.validate();
		panel.repaint();	
		
		BiochamMenuBar.refreshMenusContext(model);
		
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
				if(!((JButton)comps[i]).getActionCommand().equals("addInitConc")){
					cs.add(comps[i]);
				}
			}else{
				if(!comps[i].getName().equals("refresh")){
					cs.add(comps[i]);
				}
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
	
	public int getSavedResponses() {
		return savedResponses;
	}

	public void setSavedResponses(int savedResponses) {
		this.savedResponses = savedResponses;
	}

	public void resetSavedResponses() {
		savedResponses=-1;
	}

	public boolean isNotToAdd() {
		return notToAdd;
	}

	public void setNotToAdd(boolean notToAdd) {
		this.notToAdd = notToAdd;
	}

	public void disposeElements() {
		
		
		getInitConcentrations().clear();
		setInitConcentrations(null);
		biocham=null;
		panel=null;
		model=null;
		element=null;
		savedResponses=0;
		origValues.clear();
		origValues=null;
		
	}

	public ArrayList<String> getOrigValues() {
		return origValues;
	}	
	
	
	/**
	 * Add clear init state(delete parameters, Attention: It will delete also the parameters and macros.)
	 * add make_present_not_abset and similar, to reflect the gui also...
	 * undefinied to finish 
	 * 
	 * */
	
	
	public void actionPerformed(ActionEvent e) {
			if(e.getActionCommand()=="addInitConc"){				
				addParameter();
				BiochamMenuBar.refreshMenusContext(model);
			}else if(e.getActionCommand().equals("allAbsent")){
				bTip.setVisible(false);
				dontCheck=true;
				Component[] comps=panel.getComponents();
				for(int i=0;i<getInitConcentrations().size();i++){
					model.sendToBiocham("present("+getInitConcentrations().get(i).getName()+",0).\n");
					boolean d1=false,d2=false;
					for(int j=0;j<comps.length;j++){
						if(comps[j] instanceof JFormattedTextField && ((JFormattedTextField)comps[j]).getName().equals(getInitConcentrations().get(i).getName())){
							((JFormattedTextField)comps[j]).setEditable(true);
							((JFormattedTextField)comps[j]).setText("0.0");
							((JFormattedTextField)comps[j]).setEditable(false);	
							bTip.setVisible(false);
							d1=true;
						}
						if(comps[j] instanceof JButton && ((JButton)comps[j]).getName().equals(getInitConcentrations().get(i).getName()) && ((JButton)comps[j]).getText().equals("Absent")){
							((JButton)comps[j]).setText("Present");
							d2=true;
						}
						if(d1 && d2){
							break;
						}
					}
				}
				comps=null;
			}else if(e.getActionCommand().equals("allPresent")){
				dontCheck=true;
				bTip.setVisible(false);
				Component[] comps=panel.getComponents();
				for(int i=0;i<getInitConcentrations().size();i++){
					model.sendToBiocham("present("+getInitConcentrations().get(i).getName()+",1).\n");
					boolean d1=false,d2=false;
					for(int j=0;j<comps.length;j++){
						if(comps[j] instanceof JFormattedTextField && ((JFormattedTextField)comps[j]).getName().equals(getInitConcentrations().get(i).getName())){
							((JFormattedTextField)comps[j]).setEditable(true);
							((JFormattedTextField)comps[j]).setText("1.0");
							((JFormattedTextField)comps[j]).setEditable(false);	
							bTip.setVisible(false);
							d1=true;
						}
						if(comps[j] instanceof JButton && ((JButton)comps[j]).getName().equals(getInitConcentrations().get(i).getName()) && ((JButton)comps[j]).getText().equals("Present")){
							((JButton)comps[j]).setText("Absent");
							d2=true;
						}
						if(d1 && d2){
							break;
						}
					}
				}
				comps=null;
			}else if(e.getActionCommand().equals("deleteParameters")){
				int rep=JOptionPane.showConfirmDialog(BiochamMainFrame.frame,"This will delete all the parameters and macros also. \nAre you sure you want to continue?","Confirm",JOptionPane.YES_NO_OPTION);
				if(rep==JOptionPane.YES_OPTION){
					model.sendToBiocham("clear_initial_state.\n");
					this.getInitConcentrations().clear();
					
					((ParamTableMacros)model.getMacros().getParamTable()).macros.clear();
					((ParamTableParameters)model.getParameters().getParamTable()).parameters.clear();
					ArrayList cs=new ArrayList(0);
					panel.removeAll();			
					panel=refreshPanel(cs);	
					cs.clear();					
					panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));	
					panel.revalidate();
					((ParamTableMacros)model.getMacros().getParamTable()).panel.removeAll();
					((ParamTableMacros)model.getMacros().getParamTable()).panel=((ParamTableMacros)model.getMacros().getParamTable()).refreshPanel(cs);
					
					((ParamTableMacros)model.getMacros().getParamTable()).panel.setPreferredSize(new Dimension(((ParamTableMacros)model.getMacros().getParamTable()).getPanelCurrentX(),((ParamTableMacros)model.getMacros().getParamTable()).getPanelCurrentY()));	
					((ParamTableMacros)model.getMacros().getParamTable()).panel.revalidate();
					
					((ParamTableParameters)model.getParameters().getParamTable()).panel.removeAll();
					((ParamTableParameters)model.getParameters().getParamTable()).panel=((ParamTableParameters)model.getParameters().getParamTable()).refreshPanel(cs);						
					((ParamTableParameters)model.getParameters().getParamTable()).panel.setPreferredSize(new Dimension(((ParamTableParameters)model.getParameters().getParamTable()).getPanelCurrentX(),((ParamTableParameters)model.getParameters().getParamTable()).getPanelCurrentY()));	
					((ParamTableParameters)model.getParameters().getParamTable()).panel.revalidate();
					
					BiochamMenuBar.refreshMenusContext(model);
				}
			}else if(e.getActionCommand().equals("Undefined")){
				if(e.getSource() instanceof JButton){
					JButton b=(JButton)e.getSource();
					model.sendToBiocham("undefined("+b.getName()+").\n");
					getInitConcentrations().remove(indexOf(b.getName()));
					
					Component[] comps=panel.getComponents();
					ArrayList<Component> toRemove=new ArrayList<Component>(5);
					for(int i=0;i<comps.length;i++){
						if(comps[i].getName().equals(b.getName())){
							toRemove.add(comps[i]);
						}
					}
					for(int i=0;i<5;i++){
						panel.remove(toRemove.get(i));
					}
					refreshAfterAddingNew();
				}
			}else if(e.getActionCommand().equals("presentNotAbsent")){
				model.sendToBiocham("make_present_not_absent.\n");
			}else if(e.getActionCommand().equals("absentNotPresent")){
				model.sendToBiocham("make_absent_not_present.\n");
			}
	}

	public void removeUndefinedFromGUI(String name){
		Component[] comps=panel.getComponents();
		ArrayList<Component> toRemove=new ArrayList<Component>(5);
		for(int i=0;i<comps.length;i++){
			if(comps[i].getName().equals(name)){
				toRemove.add(comps[i]);
			}
		}
		for(int i=0;i<5;i++){
			panel.remove(toRemove.get(i));
		}
		refreshAfterAddingNew();
	}
	
	public void addParameter() {
		
		setNewAdded(true);
		DialogAddParameter param=new DialogAddParameter(BiochamMainFrame.frame,model.getInitConditions(),null,null,panel);	
		Object[] newParam=new Object[2];
		newParam=param.getNewParameter();
		String name=(String)newParam[0];
		String value=(String)newParam[1];
		param=null;
		newParam=null;    				
		if(name!=null && value!=null){
			    					
			int i=indexOf(name);    					
			if(i<0){
				
				String s="present("+name+","+value+").\n";
				model.sendToBiocham(s,"initConc");
				
			}else{
					
					String butName="Present";
					double val=0;
					try{
						val=Double.parseDouble(value);
					}catch(Exception ee){
						 /*System.out.println("The value not of numerical type...."+val+"?");
						 WorkbenchToolBars.infoLabel.setText("The value is not of numerical type...."+val+"?");
						 WorkbenchToolBars.infoLabel.setBalloonToolTipVisible(true);*/
					}			    	
			    	if(val>0){
			    		butName="Absent";
			    	}
			    	Component[] comps=panel.getComponents();	
			    	for(int k=0;k<comps.length;k++){
						  
						if(comps[k].getName().equals(name) && comps[k] instanceof JButton){
							((JButton)panel.getComponent(k+1)).setText(butName);
							break;
						}
			    	}
		 			
		 			comps=panel.getComponents();	
					for(int k=0;k<comps.length;k++){
										  
						if(comps[k].getName().equals(name) && comps[k] instanceof JFormattedTextField){
							
							((JFormattedTextField)comps[k]).setValue(value);
							setModified(((JFormattedTextField)comps[k]));
							setNotToAdd(true);
							model.sendToBiocham("present("+name+","+value+").\n"+"list_molecules.\n","initConc");
							getInitConcentrations().remove(i);
							getInitConcentrations().add(i,new InitialConcentration(name,value));
							((JFormattedTextField)comps[k]).setEditable(false);
							comps=null;
							break;
						}
					}			
			}
		}
	}

	public boolean isNewAdded() {
		return newAdded;
	}

	public void setNewAdded(boolean newAdded) {
		this.newAdded = newAdded;
	}

	public boolean isModifying() {
		return modifying;
	}

	public void setModifying(boolean modifying) {
		this.modifying = modifying;
	}

	public void removeLastAdded() {
		resetParameter(lastAdded);	
		
	}

	public boolean isDontCheck() {
		return dontCheck;
	}

	public void setDontCheck(boolean dontCheck) {
		this.dontCheck = dontCheck;
	}
	public void setInitConcentrations(Vector<InitialConcentration> initConcentrations) {
		this.initConcentrations = initConcentrations;
	}
	public Vector<InitialConcentration> getInitConcentrations() {
		return initConcentrations;
	}

	
}
