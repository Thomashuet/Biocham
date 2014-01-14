package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.BiochamModelElement;
import fr.inria.contraintes.biocham.WorkbenchArea;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.customComponents.DeleteButton;
import fr.inria.contraintes.biocham.customComponents.ModifyButton;
import fr.inria.contraintes.biocham.dialogs.DialogAddParameter;
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
import java.util.HashMap;
import java.util.Vector;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.Spring;
import javax.swing.SpringLayout;



public class ParamTableParameters implements Parameters, ActionListener{
	
	
	
	public Vector<Parameter> parameters;
	WorkbenchArea biocham;
	JPanel panel;
	ParametersModel parametersModel;
	ParametersView view;
	

	public ParametersModel getParametersModel() {
		return parametersModel;
	}
	public void setParametersModel(ParametersModel parametersModel) {
		this.parametersModel = parametersModel;
	}


	BiochamModel model;
	BiochamModelElement element;
	int savedResponses;
	boolean notToAdd=false;	
	ArrayList<String> uknown;
	HashMap<Integer,String> origValues;
	BalloonTip bTip=null;
	boolean newAdded=false;
	String lastAdded="";
	boolean modifying=false;
	boolean dontAskForModify=false;

	
	public boolean isDontAskForModify() {
		return dontAskForModify;
	}


	public ParamTableParameters(BiochamModel m,WorkbenchArea workbench, JPanel p){
		
		biocham=workbench;
		model=m;
		panel=p;
		parameters=new Vector<Parameter>();
		savedResponses=-1;
		origValues=new HashMap<Integer,String>();
		uknown=new ArrayList<String>();
		Utils.modern.setBorderThickness(3);
		Utils.modern.enableAntiAliasing(true);	
		
		parametersModel=new ParametersModel(model);
		view=new ParametersView(BiochamMainFrame.frame,parametersModel);
	}
	
	
	public ParametersView getView() {
		return view;
	}
	public void setView(ParametersView view) {
		this.view = view;
	}
	public String getName(int i) {
		return parameters.get(i).getName();
	}

	public String getValue(int i) {
		return parameters.get(i).getValue();
	}

	public int indexOf(String paramName) {
		
		 int i=0;
	     while (i<parameters.size() && !getName(i).equals(paramName))
	         i++;
	     if (i == parameters.size())
	         return -1;
	     return i;
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
	
	public void resetParameter(String s) {
		
		int i = indexOf(s);
		JFormattedTextField tf = ((JFormattedTextField) panel.getComponent(4*i+1));
		setSavedResponses(-1);
		if(getOrigValues().size()>i-1){
			if(getOrigValues().get(i)!=null){
				if(!getValue(i).equals(getOrigValues().get(i))){
					
					if (i>=0) {
									
						String ovalue=getOrigValues().get(i);				
					    tf.setValue(ovalue);
					    tf.setBackground(Color.WHITE);
					    String s1="parameter("+s+","+ovalue+").\n";
					    setNotToAdd(true);
						model.sendToBiocham(s1,"params");	
						parameters.remove(i);
						parameters.add(i,new Parameter(s,ovalue));
						refreshAfterAddingNew();
					}
				}else{
					 tf.setBackground(Color.WHITE);
				}
			}
		}
		tf=null;
	}
	
	public void addUknownParameter(ArrayList<String> list){
		
		String arg1,arg2;
		arg1=(String)list.get(0);
		arg2=(String)list.get(1);
				
		uknown.add(arg1);
		
		JFormattedTextField tf=new JFormattedTextField();
		tf.setColumns(10);
		JLabel redAsterisk=new JLabel(Icons.icons.get("redAsterisk.gif"+0.01));
		redAsterisk.setName("redAsterisk");
		Component[] comps=panel.getComponents();  
	
		for(int k=0;k<comps.length;k++){
			if(comps[k].getName().equals("Add")){
        		panel.remove(k);	       		 	
        	}
		}
		Spring maxSpring = Spring.constant(MIDDLE);
		SpringLayout layout = (SpringLayout) panel.getLayout();
		JLabel l = new JLabel(arg1);
		l.setName(arg1);
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
							scrC=null;
							String name=tf.getName();				
							if(e.getKeyCode()==e.VK_ENTER){
								Component[] comps=panel.getComponents();	
								for(int k=0;k<comps.length;k++){
													  
									if(comps[k].getName().equals(name) && comps[k] instanceof JFormattedTextField){
										
										((JFormattedTextField)comps[k]).setValue(tf.getText());										
										((JFormattedTextField)comps[k]).setEditable(false);
										setNotToAdd(true);
										lastAdded=name;
										model.sendToBiocham("parameter("+name+","+tf.getText()+").\n","params");										
										parameters.add(new Parameter(name,tf.getText()));
										boolean removed=uknown.remove(name);
										removed=((ParamTableRules)model.getRules().getParamTable()).getUknownParams().remove(name);
										ArrayList<Component> cs=new ArrayList<Component>();
										comps=panel.getComponents();
										for(int i=0;i<comps.length;i++){
											
											String s=comps[i].getName();
											if(!s.equals("Add") && !s.equals("refresh") && !s.equals("redAsterisk")){
												cs.add(comps[i]);
											}
										}
										setModifying(false);
										panel.removeAll();			
										panel=refreshPanel(cs);			
										cs.clear();
										comps=null;
										cs=null;									
										panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));
								    				 
										return;
									}
								}
							}
						}
					}
				}
			}
		});
		tf.setBackground(Color.white);	     
		ModifyButton but1=new ModifyButton();	
    	but1.setName(arg1);
    	but1.addMouseListener(new MouseAdapter(){
    		
    		
			public void mouseClicked(MouseEvent e) {
				setModifying(true);
				setSavedResponses(-1);
				Component button=(Component) e.getSource();
				String name=button.getName();
				JPanel panel=(JPanel) button.getParent();
			    Component[] comps=panel.getComponents();
			    Vector cs=new Vector();
			    for(int i=0;i<comps.length;i++){
			    	if(comps[i].getName()==name){
			    		cs.add(comps[i]);		    		
			    	}
			    }
			    JFormattedTextField tf= (JFormattedTextField) cs.get(1);
			    tf.setEditable(true);
			    cs.clear();
			    cs=null;
			    tf=null;
			    button=null;
			    comps=null;
			    panel=null;
			}

			
    	});
    	
    	DeleteButton b2=new DeleteButton();
    	b2.setName(arg1);
    	b2.addMouseListener(new MouseAdapter(){

			public void mouseClicked(MouseEvent e) {
				Component button=(Component) e.getSource();
				String name=button.getName();
				int k=indexOf(name);
				parameters.remove(k);
			
				getOrigValues().remove(k);
				String s="delete_parameter("+name+").\n";	    				
				model.sendToBiocham(s,"params");	    				
				JPanel panel=(JPanel) button.getParent();			
			    Component[] comps=panel.getComponents();
			    ArrayList cs=new ArrayList();   			    
			    for(int i=0;i<comps.length;i++){
			    	if(comps[i].getName()==name){
			    		cs.add(comps[i]);	    		
			    	}
			    }
			    
			    JLabel label=(JLabel)cs.get(0);
			    JFormattedTextField tf= (JFormattedTextField) cs.get(1);
			    JLabel but1=(JLabel)cs.get(2);
			    JLabel but2=(JLabel)cs.get(3);
			    panel.remove(label);
			    panel.remove(tf);
			    panel.remove(but1);
			    panel.remove(but2);
			    comps=null;
			    comps=panel.getComponents();
			    cs.clear();
			    for(int i=0;i<comps.length;i++){	
			    	if((comps[i] instanceof JLabel)){
						if(!((JButton)comps[i]).getActionCommand().equals("addParameter")){
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
			    panel.removeAll();
			    refreshPanel(cs);
			    cs.clear();	    			  
			    panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));	
			    panel.getParent().validate();
				panel.getParent().repaint();
				cs=null;
			    comps=null;
			    button=null;
			    panel=null;
			    label=null;
			    tf=null;
			    but1=null;
			    but2=null;
			    
			    BiochamMenuBar.refreshMenusContext(model);
			}});
    	
    	
    	panel.add(l);
    	panel.add(tf);
    	panel.add(redAsterisk);
    	panel.add(but1);  
    	panel.add(b2); 
    	
    	parameters.add(new Parameter(arg1,arg2));
    	
    	int numParam = parameters.size()-1;
    	layout.putConstraint(SpringLayout.WEST, l, LEFT_OFF,SpringLayout.WEST, panel);
    	layout.putConstraint(SpringLayout.NORTH, l, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);  	
    	layout.putConstraint(SpringLayout.WEST, but1, 5, SpringLayout.EAST, tf);	     
    	layout.putConstraint(SpringLayout.NORTH, but1, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);    
    	layout.putConstraint(SpringLayout.WEST, b2, LEFT_OFF, SpringLayout.EAST, but1);	     
    	layout.putConstraint(SpringLayout.NORTH, b2, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);      	
    	layout.putConstraint(SpringLayout.NORTH, tf, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);         
    
    	//redAsterisk       
    	layout.putConstraint(SpringLayout.NORTH, redAsterisk, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);         
    	layout.putConstraint(SpringLayout.WEST, redAsterisk, 1 ,SpringLayout.EAST, tf);
     
    	for (int i=0;i<parameters.size();++i){
			if(panel.getComponent(i) instanceof JLabel && !panel.getComponent(i).getName().equals("redAsterisk")){
				maxSpring = Spring.max(maxSpring, Spring.sum(layout.getConstraints(panel.getComponent(i)).getWidth(),Spring.constant(10)));
			}
	    }
	    for (int i=0;i<parameters.size();++i){
	    	if(panel.getComponent(i) instanceof JFormattedTextField){
	    		layout.putConstraint(SpringLayout.WEST, panel.getComponent(i), maxSpring, SpringLayout.WEST, panel);
	    	}
	    	
	    } 
	    
		refreshAfterAddingNew();
	    panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));	
	    panel.revalidate();
	}
	
	
	
	public void setValue(ArrayList<String> list) {
		
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
			
			Component[] comps=panel.getComponents();
			parametersModel.addParameter(arg1, arg2);
			for(int k=0;k<comps.length;k++){
				if(comps[k].getName().equals("Add")){
	        		panel.remove(k);	       		 	
	        	}
			}			
			parameters.add(new Parameter(arg1,arg2));
			
			int numParam = parameters.size()-1;
			boolean exists=false;
			for(int y=0;y<getOrigValues().size();y++){
				if(numParam==y){
					exists=true;
					break;
				}
			}
			if(!exists){
				getOrigValues().put(numParam,arg2);
			}			

			Spring maxSpring = Spring.constant(MIDDLE);
			SpringLayout layout = (SpringLayout) panel.getLayout();
			JLabel l = new JLabel(arg1);
			l.setName(arg1);  
			    
			

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
								scrC=null;
								String name=tf.getName();
								
								if(e.getKeyCode()==e.VK_ENTER){
									
								    int i=indexOf(name);
								    if(i>=0){
								    		
								    		Component[] comps=panel.getComponents();	
											for(int k=0;k<comps.length;k++){
																  
												if(comps[k].getName().equals(name) && comps[k] instanceof JFormattedTextField){
													
													((JFormattedTextField)comps[k]).setValue(tf.getText());
													setModified(((JFormattedTextField)comps[k]));
													((JFormattedTextField)comps[k]).setEditable(false);
													setNotToAdd(true);
													model.sendToBiocham("parameter("+name+","+tf.getText()+").\n","params");
													lastAdded=name;
													parameters.remove(i);
													parameters.add(i,new Parameter(name,tf.getText()));
													refreshAfterAddingNew();
													comps=null;
													return;
												}
											}								    		
								    }
								}
							}
						}
					}		
			   }
			}); //On click ENTER call the action that modifies the value
	    	tf.setBackground(Color.white);
	     
	    	ModifyButton but1=new ModifyButton();	
	    	but1.setName(arg1);
	    	but1.addMouseListener(new MouseAdapter(){

				public void mouseClicked(MouseEvent e) {
					setModifying(true);
    				setSavedResponses(-1);
    				Component button=(Component) e.getSource();
    				String name=button.getName();
    				JPanel panel=(JPanel) button.getParent();//arg1
    			    Component[] comps=panel.getComponents();
    			    Vector cs=new Vector();
    			    for(int i=0;i<comps.length;i++){
    			    	if(comps[i].getName()==name){
    			    		cs.add(comps[i]);	//label+tf+Modify button....		    		
    			    	}
    			    }
    			    JFormattedTextField tf= (JFormattedTextField) cs.get(1);
    			    tf.setEditable(true);
    			    cs.clear();
    			    cs=null;
    			    tf=null;
    			    button=null;
    			    comps=null;
    			    panel=null;
				}
	    	});
	    
	    	DeleteButton b2=new DeleteButton();
	    	b2.setName(arg1);
	    	b2.addMouseListener(new MouseAdapter(){

	    		public void mouseClicked(MouseEvent e){
	    			Component button=(Component) e.getSource();
    				String name=button.getName();
    				int k=indexOf(name);
    				parameters.remove(k);
    			
    				getOrigValues().remove(k);
    				String s="delete_parameter("+name+").\n";	    				
					model.sendToBiocham(s,"params");	    				
    				JPanel panel=(JPanel) button.getParent();			
    			    Component[] comps=panel.getComponents();
    			    ArrayList cs=new ArrayList();   			    
    			    for(int i=0;i<comps.length;i++){
    			    	if(comps[i].getName()==name){
    			    		cs.add(comps[i]);	    		
    			    	}
    			    }
    			    
    			    JLabel label=(JLabel)cs.get(0);
    			    JFormattedTextField tf= (JFormattedTextField) cs.get(1);
    			    JLabel but1=(JLabel)cs.get(2);
    			    JLabel but2=(JLabel)cs.get(3);
    			    panel.remove(label);
    			    panel.remove(tf);
    			    panel.remove(but1);
    			    panel.remove(but2);
    			    comps=null;
    			    comps=panel.getComponents();
    			    cs.clear();
    			    for(int i=0;i<comps.length;i++){	
    			    	if((comps[i] instanceof JButton)){
							if(!((JButton)comps[i]).getActionCommand().equals("addParameter")){
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
    			    panel.removeAll();
    			    refreshPanel(cs);
    			    cs.clear();	    		
    			    panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));	
    			    panel.getParent().validate();
    				panel.getParent().repaint();
    				cs=null;
    			    comps=null;
    			    button=null;
    			    panel=null;
    			    label=null;
    			    tf=null;
    			    but1=null;
    			    but2=null;
    			    BiochamMenuBar.refreshMenusContext(model);
			    
	    		}
				});
	    	
	    	panel.add(l);
	    	panel.add(tf);
	    	panel.add(but1);
	    	panel.add(b2);    	
	    	layout.putConstraint(SpringLayout.WEST, l, LEFT_OFF,SpringLayout.WEST, panel);
	    	layout.putConstraint(SpringLayout.NORTH, l, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);       	
	    	layout.putConstraint(SpringLayout.WEST, but1, 5, SpringLayout.EAST, tf);	     
	    	layout.putConstraint(SpringLayout.NORTH, but1, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);  
	    	layout.putConstraint(SpringLayout.WEST, b2, LEFT_OFF, SpringLayout.EAST, but1);	     
	    	layout.putConstraint(SpringLayout.NORTH, b2, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);  	     
	    	layout.putConstraint(SpringLayout.NORTH, tf, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);         
	    
	    	for (i=0;i<parameters.size();++i){
				if(panel.getComponent(i) instanceof JLabel && !panel.getComponent(i).getName().equals("redAsterisk") && !panel.getComponent(i).getName().equals("refresh")){
					maxSpring = Spring.max(maxSpring, Spring.sum(layout.getConstraints(panel.getComponent(i)).getWidth(),Spring.constant(10)));
				}
		    }
		    for (i=0;i<parameters.size();++i){
		    	if(panel.getComponent(i) instanceof JFormattedTextField){
		    		layout.putConstraint(SpringLayout.WEST, panel.getComponent(i), maxSpring, SpringLayout.WEST, panel);
		    	}
		    }
	     	       			
	    	maxSpring=null;
			
		}else{
			if(!((ParamTableRules)model.getRules().getParamTable()).isIgnoreUndefinedParametersWarnings() && !this.isDontAskForModify()){
				
					setSavedResponses(0);
					Component[] comps=panel.getComponents();	
					for(int k=0;k<comps.length;k++){
										  
						if(comps[k].getName().equals(arg1) && comps[k] instanceof JFormattedTextField){
							
							((JFormattedTextField)comps[k]).setValue(arg2);
							setModified(((JFormattedTextField)comps[k]));
							parameters.remove(i);
							parameters.add(i,new Parameter(arg1,arg2));
							((JFormattedTextField)comps[k]).setEditable(false);
							comps=null;
							return;
						}
					}								
			}else{
				Component[] comps=panel.getComponents();	
				for(int k=0;k<comps.length;k++){
									  
					if(comps[k].getName().equals(arg1) && comps[k] instanceof JFormattedTextField){
						
						if(this.isDontAskForModify()){
							setModified(((JFormattedTextField)comps[k]));		
						}
						((JFormattedTextField)comps[k]).setValue(arg2);						
						parameters.remove(i);
						parameters.add(i,new Parameter(arg1,arg2));
						((JFormattedTextField)comps[k]).setEditable(false);
						comps=null;
						return;
					}
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
	

	public int size() {		
		return parameters.size();
	}
	
	public void setModified(Component comp) {
		comp.setBackground(Utils.modifiedParameterColor);	
	}
	
	
	
	 /**
	    * An inner class that represents the parameter object.
	    *       
	    * @author Sylvain Soliman         
	   */
		public class Parameter {
	      
		  private String name;
	      private String value;
	      
	      
	      Parameter(String s, String k) {
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
		
		int rows = cs.size()/4;	
		
				
		for(int i=0;i<rows;i++){
			 
			JComponent cp=(JComponent) cs.get(4*i);
			JLabel l=(JLabel)cp;
			
			JFormattedTextField tf= (JFormattedTextField) cs.get(4*i+1);
			JLabel asterisk=null;
			if(tf.getValue().equals("You have to define the value of this parameter...")){	
				asterisk=new JLabel(Icons.icons.get("redAsterisk.gif"+0.01));
				asterisk.setName("redAsterisk"); 
				
			}
			
			JLabel but1=(JLabel)cs.get(4*i+2);			
			JLabel but2=null;
			
			panel.add(l);
			panel.add(tf);
			if(asterisk!=null){
				panel.add(asterisk);
				if(cs.get(4*i+3) instanceof JLabel){
					but2=(JLabel)cs.get(4*i+3);
				}else{
					if(cs.get(4*i+4) !=null){
						if(cs.get(4*i+4) instanceof JLabel){
							but2=(JLabel)cs.get(4*i+4);
						}
					}
				}
					
			}else{
				but2=(JLabel)cs.get(4*i+3);
			}
			
			panel.add(but1);	
			panel.add(but2);
			layout.putConstraint(SpringLayout.WEST, l, LEFT_OFF, SpringLayout.WEST, panel);
			layout.putConstraint(SpringLayout.NORTH, l, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);
			layout.putConstraint(SpringLayout.WEST, but1, 5, SpringLayout.EAST, tf);	     
	    	layout.putConstraint(SpringLayout.NORTH, but1,  TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel); 
	    	layout.putConstraint(SpringLayout.WEST, but2, LEFT_OFF, SpringLayout.EAST, but1);	     
	    	layout.putConstraint(SpringLayout.NORTH, but2,  TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel); 	     
	    	layout.putConstraint(SpringLayout.NORTH, tf, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);     
			
		    //redAsterisk
		    if(asterisk!=null){
		    	layout.putConstraint(SpringLayout.NORTH, asterisk, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);        
		    	layout.putConstraint(SpringLayout.WEST, asterisk, 1 ,SpringLayout.EAST, tf);
		    }
		    
		    cp=null;
		    l=null;
		    tf=null;
		    but1=null;
		    asterisk=null;
		   
		}
		
		rows=panel.getComponentCount();
		for (int i=0;i<rows;++i){
			if(panel.getComponent(i) instanceof JLabel && !panel.getComponent(i).getName().equals("redAsterisk")){
				maxSpring = Spring.max(maxSpring, Spring.sum(layout.getConstraints(panel.getComponent(i)).getWidth(),Spring.constant(10)));
			}
	    }
	    for (int i=0;i<rows;++i){
	    	if(panel.getComponent(i) instanceof JFormattedTextField){
	    		layout.putConstraint(SpringLayout.WEST, panel.getComponent(i), maxSpring, SpringLayout.WEST, panel);
	    	}
	    }
	    
	    maxSpring=null;
	    
	    String toolTipText="<html><i>Declare a parameter.<br> Example: parameter(k1,4.0675).</i></html>";
	    CustomToolTipButton b1=new CustomToolTipButton("Add",toolTipText);	
	    b1.setName("Add");
	    b1.setActionCommand("addParameter");
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
	    panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));	
	    panel.validate();
		panel.repaint();	
		b1=null;
		b2=null;
		layout=null;
		return panel;
	}



	/**
	 * 
	 */
	public void refreshAfterAddingNew() {
		
		ArrayList<Component> cs=new ArrayList<Component>();
		Component[] comps=panel.getComponents();
		for(int i=0;i<comps.length;i++){
			
			String s=comps[i].getName();
			if(!s.equals("Add") && !s.equals("refresh") && !s.equals("redAsterisk")){
				cs.add(comps[i]);
			}					
		}
		panel.removeAll();			
		panel=refreshPanel(cs);			
		cs.clear();
		comps=null;
		cs=null;
		panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));	
		
	}

	public int getSavedResponses() {
		return savedResponses;
	}	
	public void clearSavedResponses() {
		this.savedResponses=0;
	}
	public void setSavedResponses(int res) {
		this.savedResponses = res;
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
		
		parameters.clear();
		parameters=null;
		biocham=null;
		panel=null;
		model=null;
		element=null;
		savedResponses=0;
		origValues.clear();
		origValues=null;
		uknown.clear();
		uknown=null;
	}


	public HashMap<Integer,String> getOrigValues() {
		return origValues;
	}


	public ArrayList<String> getUknown() {
		return uknown;
	}


	public void setUknown(ArrayList<String> uknown) {
		this.uknown = uknown;
	}


	public void actionPerformed(ActionEvent e) {
		if(e.getActionCommand()=="addParameter"){
			
			addParameter();
			BiochamMenuBar.refreshMenusContext(model);
			
		}else if(e.getActionCommand().equals("deleteAll")){
			for(int i=0;i<parameters.size();i++){
				model.sendToBiocham("delete_parameter("+parameters.get(i).getName()+").\n");
			}
			parameters.clear();
			
			panel.removeAll();	
		
			ArrayList cs=new ArrayList(0);
			panel=refreshPanel(cs);			
			cs.clear();
			cs=null;			
			panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));	
			panel.revalidate();
			
			BiochamMenuBar.refreshMenusContext(model);
		}else if(e.getActionCommand().equals("listDimensions")){
			listDimensions();
		}
		else if(e.getActionCommand().equals("setDimension")){
			setDimension();
		}
	}

	public void listDimensions(){}
	public void setDimension(){}
	
	
	public void addParameter() {
		
		setNewAdded(true);
		setSavedResponses(-1);
		DialogAddParameter param=new DialogAddParameter(BiochamMainFrame.frame,model.getParameters(),null,null,panel);	
		Object[] newParam=new Object[2];
		newParam=param.getNewParameter();
		String name=(String)newParam[0];
		String value=(String)newParam[1];
		param=null;
		newParam=null;
		
		if(name!=null && value!=null){
			
			int i=indexOf(name);
			if(i<0){    					
				String s="parameter("+name+","+value+").\n";	    					
				model.sendToBiocham(s,"params");
			}else{
				
				if(!this.isDontAskForModify()){
					
					Component[] comps=panel.getComponents();	
					for(int k=0;k<comps.length;k++){
										  
						if(comps[k].getName().equals(name) && comps[k] instanceof JFormattedTextField){
							
							((JFormattedTextField)comps[k]).setValue(value);
							setModified(((JFormattedTextField)comps[k]));
							setNotToAdd(true);
							model.sendToBiocham("parameter("+name+","+value+").\n","params");
							parameters.remove(i);
							parameters.add(i,new Parameter(name,value));
							((JFormattedTextField)comps[k]).setEditable(false);
							comps=null;
							break;
						}
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


	public void setDontAskForModify(boolean b) {
		dontAskForModify=b;
		
	}


	public Vector<Parameter> getParameters() {
		return parameters;
	}

}
