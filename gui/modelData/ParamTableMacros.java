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
import java.util.Vector;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.Spring;
import javax.swing.SpringLayout;

public class ParamTableMacros implements Parameters, ActionListener{

	
	public Vector<Macro> macros;
	WorkbenchArea biocham;
	JPanel panel;
	BiochamModel model;
	BiochamModelElement element;
	int savedResponses;
	boolean notToAdd=false;
	boolean modifying=false;
	ArrayList<String> origValues;
	BalloonTip bTip;
	boolean newAdded=false;
	String lastAdded="";
	boolean modiff=false;

	MacrosModel macrosModel;
	MacrosView view;
	
	public MacrosView getView() {
		return view;
	}
	public void setView(MacrosView view) {
		this.view = view;
	}
	public MacrosModel getMacrosModel() {
		Utils.debugMsg("macros size="+macrosModel.getMacros().size());
		return macrosModel;
	}
	public void setMacrosModel(MacrosModel macrosModel) {
		this.macrosModel = macrosModel;
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
	
	public ParamTableMacros(BiochamModel m,WorkbenchArea workbench, JPanel p){
		
		biocham=workbench;
		model=m;
		panel=p;
		macros=new Vector<Macro>();
		savedResponses=-1;
		origValues=new ArrayList<String>();
		Utils.modern.setBorderThickness(3);
		Utils.modern.enableAntiAliasing(true);	
		
		macrosModel=new MacrosModel(model);
		view=new MacrosView(BiochamMainFrame.frame,macrosModel);
	}
	

	public String getName(int i) {
		return macros.get(i).getName();
	}


	public String getValue(int i) {
		return macros.get(i).getValue();
	}


	public int indexOf(String paramName) {

		 int i=0;
	     while (i<macros.size() && !getName(i).equals(paramName))
	         i++;
	     if (i == macros.size())
	         return -1;
	     return i;
		
	}


	public void resetParameter(String s) {
		
		int i = indexOf(s);
		JFormattedTextField tf = ((JFormattedTextField) panel.getComponent(4*i+1));
		setSavedResponses(-1);
		setModifying(true);
		
		
		if(!getValue(i).equals(getOrigValues().get(i))){
			
			if (i>=0) {
							
				String ovalue=getOrigValues().get(i);				
			    tf.setValue(ovalue);
			    tf.setBackground(Color.WHITE);
			    String s1="macro("+s+","+ovalue+").\n";
			    setNotToAdd(true);
				model.sendToBiocham(s1,"macros");	
				macros.remove(i);
				macros.add(i,new Macro(s,ovalue));
				refreshAfterAddingNew();
			}
		}else{
			 tf.setBackground(Color.WHITE);
		}
		tf=null;
		
	}


	public void setValue(ArrayList list) {
		
		String arg1,arg2;
		arg1=(String)list.get(0);
		arg2=(String)list.get(1);		
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
				
				if(comps[k].getName().contains("Add") ){
	        		panel.remove(k);	       		 	
	        	}
			}
			Macro macro=new Macro(arg1,arg2);		
			macros.add(macro); //vector of parameters
			macrosModel.addMacro(arg1, arg2);
			
			int numParam = macros.size()-1;		
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
			Spring ms2=Spring.constant(MIDDLE);
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
								String name=tf.getName();
								scrC=null;
								
								if(e.getKeyCode()==e.VK_ENTER){
										
									setSavedResponses(-1);
								    int i=indexOf(name);
								    if(i>=0){
								    									    		
								    		Component[] comps=panel.getComponents();	
											for(int k=0;k<comps.length;k++){
																  
												if(comps[k].getName().equals(name) && comps[k] instanceof JFormattedTextField){
													
													((JFormattedTextField)comps[k]).setValue(tf.getText());
													setModified(((JFormattedTextField)comps[k]));
													((JFormattedTextField)comps[k]).setEditable(false);
													setNotToAdd(true);
													lastAdded=name;
													model.sendToBiocham("macro("+name+","+tf.getText()+").\n","macros");
													macros.remove(i);
													macros.add(i,new Macro(name,tf.getText()));
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
			});//(model); //On click ENTER call the action that modifies the value
	    	tf.setBackground(Color.white);
	     
	    	ModifyButton but1=new ModifyButton();	
	    	but1.setName(arg1);
	    	but1.addMouseListener(new MouseAdapter(){

				public void mouseClicked(MouseEvent e) {
					setModifying(true);
    				setModiff(true);
    				setSavedResponses(-1);
    				Component button=(Component) e.getSource();
    				String name=button.getName();
    				JPanel panel=(JPanel) button.getParent();//arg1
    			    Component[] comps=panel.getComponents();
    			    Vector cs=new Vector();
    			    for(int i=0;i<comps.length;i++){
    			    	if(comps[i].getName().equals(name)){
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

	    		public void mouseClicked(MouseEvent e){
	    			Component button=(Component) e.getSource();
    				String name=button.getName();
    				int k=indexOf(name);
    				macros.remove(k);
    				
    				getOrigValues().remove(k);
    				String s="delete_macro("+name+").\n";	    				
					model.sendToBiocham(s,"macros");	    				
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
							if(!((JButton)comps[i]).getActionCommand().equals("addMacro")){
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
	    	layout.putConstraint(SpringLayout.NORTH, tf, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);  
	    	layout.putConstraint(SpringLayout.NORTH, but1, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);
	    	layout.putConstraint(SpringLayout.WEST, but1, 5, SpringLayout.EAST, tf);
	    	layout.putConstraint(SpringLayout.NORTH, b2, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);
	    	layout.putConstraint(SpringLayout.WEST, b2, LEFT_OFF, SpringLayout.EAST, but1);	     
	    	       	
	    	int max=10,temp=0;
	    	for(int j=0;j<macros.size();j++){
	    		if(panel.getComponent(4*j+1) instanceof JFormattedTextField){
	    			temp=((JFormattedTextField)panel.getComponent(4*j+1)).getColumns();
	    			if(temp>=max){
	    				max=temp;
	    			}
	    		}
	    	}
	    	for(int j=0;j<macros.size();j++){
	    		if(panel.getComponent(4*j+1) instanceof JFormattedTextField){
	    			((JFormattedTextField)panel.getComponent(4*j+1)).setColumns(max);//.setColumns(max+1);	    
	    		}
	    	}			
	    	for (i=0;i<macros.size();++i)
	    		maxSpring = Spring.max(maxSpring, Spring.sum(layout.getConstraints(panel.getComponent(4*i)).getWidth(),Spring.constant(10)));
	    	for (i=0;i<macros.size();++i)
	    		layout.putConstraint(SpringLayout.WEST, panel.getComponent(4*i+1), maxSpring, SpringLayout.WEST, panel);	    	
	    	for (i=0;i<macros.size();++i)
	    		ms2 = Spring.max(ms2, Spring.sum(layout.getConstraints(panel.getComponent(4*i+1)).getWidth(),maxSpring));
	    	for (i=0;i<macros.size();++i)
	    		layout.putConstraint(SpringLayout.WEST, panel.getComponent(4*i+2), ms2, SpringLayout.WEST, panel);
	    		
	    	
		}else{
				
				setSavedResponses(0);
				Component[] comps=panel.getComponents();	
				for(int k=0;k<comps.length;k++){
									  
					if(comps[k].getName().equals(arg1) && comps[k] instanceof JFormattedTextField){
						
						((JFormattedTextField)comps[k]).setValue(arg2);
						setModified(((JFormattedTextField)comps[k]));
						macros.remove(i);
						macros.add(i,new Macro(arg1,arg2));
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
		
	}


	public int size() {
		return macros.size();
	}
	
	public void setModified(Component comp) {
		comp.setBackground(Utils.modifiedParameterColor);	
		
	}
	
	public class Macro {
	      
		  private String name;
	      private String value;

	      Macro(String s, String k) {
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
	      // Sets the original value of this parameter    
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
			
			JLabel but1=(JLabel)cs.get(4*i+2);
			JLabel but2=(JLabel)cs.get(4*i+3);
			panel.add(l);
			panel.add(tf);
			panel.add(but1);
			panel.add(but2);					
			
			layout.putConstraint(SpringLayout.WEST, l, LEFT_OFF, SpringLayout.WEST, panel);
			layout.putConstraint(SpringLayout.NORTH, l, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);					
			layout.putConstraint(SpringLayout.NORTH, tf, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);  
			layout.putConstraint(SpringLayout.NORTH, but1, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);
			layout.putConstraint(SpringLayout.WEST, but1, 5, SpringLayout.EAST, tf);
			layout.putConstraint(SpringLayout.NORTH, but2, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);
			layout.putConstraint(SpringLayout.WEST, but2, LEFT_OFF, SpringLayout.EAST, but1);
				   
		    cp=null;
		    l=null;
		    tf=null;
		    but1=null;
		   
		}
		int max=10;
		Spring ms2=Spring.constant(MIDDLE);	
		String elem_max = null;
    	for(int j=0;j<macros.size();j++){
    		try{
    			if(panel.getComponent(4*j+1) instanceof JFormattedTextField){
    				elem_max=((JFormattedTextField)panel.getComponent(4*j+1)).getText();
    				if(elem_max.length()>=max){
    					max=elem_max.length();
    				}
    			}
    		}catch(Exception e){}
    	}
    	for(int j=0;j<macros.size();j++){
    		try{
    			if(panel.getComponent(4*j+1) instanceof JFormattedTextField){
    				((JFormattedTextField)panel.getComponent(4*j+1)).setColumns(max-max/4); 
    			}
    		}catch(Exception e){}
    	}
		
    	//System.out.println("Declarations.size="+macros.size());
    	for (int i=0;i<macros.size();++i)
    		maxSpring = Spring.max(maxSpring, Spring.sum(layout.getConstraints(panel.getComponent(4*i)).getWidth(),Spring.constant(10)));
    	for (int i=0;i<macros.size();++i)
    		layout.putConstraint(SpringLayout.WEST, panel.getComponent(4*i+1), maxSpring, SpringLayout.WEST, panel);
    	
    	for (int i=0;i<macros.size();++i)
    		ms2 = Spring.max(ms2, Spring.sum(layout.getConstraints(panel.getComponent(4*i+1)).getWidth(),Spring.sum(maxSpring,Spring.constant(10))));
    	for (int i=0;i<macros.size();++i)
    		layout.putConstraint(SpringLayout.WEST, panel.getComponent(4*i+2), ms2, SpringLayout.WEST, panel);
    	
	    String toolTipText="<html><i>Define a new macro.<br>Example: name=TOT, value=k1-k2*[X].</i></html>";
	    CustomToolTipButton b1=new CustomToolTipButton("Add",toolTipText);
	    b1.setName("Add");
	    b1.setActionCommand("addMacro");
	    b1.addActionListener(this);
	    b1.setBalloonToolTipVisible(false);
		JLabel b3=new JLabel();
		b3.setIcon(Icons.icons.get("Refresh3.png"));
		b3.setName("refresh");	    
		b3.setText("Screen Refresh");
		b3.setForeground(Utils.refreshedColor);
	    b3.setToolTipText("Click to Refresh the Screen");
	    b3.addMouseListener(new MouseAdapter(){
	    	public void mouseClicked(MouseEvent me) {
	    		              
                refreshAfterAddingNew();	
            }
	    });
		
		Component[] comps=panel.getComponents();
		
		if(comps.length>0 && !(comps[0] instanceof JButton)){
			
			 panel.add(b1);
			 panel.add(b3);
			
			 layout.putConstraint(SpringLayout.NORTH, b1,40,SpringLayout.NORTH,comps[comps.length-3]);
	         layout.putConstraint(SpringLayout.WEST, b1, LEFT_OFF, SpringLayout.WEST, panel);
	         layout.putConstraint(SpringLayout.NORTH, b3, 0,SpringLayout.NORTH, b1);
	         layout.putConstraint(SpringLayout.WEST, b3, 10, SpringLayout.EAST, b1);
	    			 
		}else{
			 panel.add(b1);
			 panel.add(b3);
			
			 layout.putConstraint(SpringLayout.NORTH, b1, HEIGHT,SpringLayout.NORTH, panel);
			 layout.putConstraint(SpringLayout.NORTH, b3, HEIGHT,SpringLayout.NORTH, panel);
			 layout.putConstraint(SpringLayout.WEST, b1, 10, SpringLayout.WEST, panel);	
			 layout.putConstraint(SpringLayout.WEST, b3, 5, SpringLayout.EAST, b1);	
		}		
		 cs.clear();
	     comps=null;
	     b1=null;
		 layout=null;	
	     panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));	
	     panel.validate();
		 panel.repaint(); 	
		 
		 //BiochamMenuBar.paramM.refreshed(model);
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
				if(!((JButton)comps[i]).getActionCommand().equals("addMacro")){
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
		
		macros.clear();
		macros=null;
		biocham=null;
		panel=null;
		model=null;
		element=null;
		savedResponses=0;
		origValues.clear();
		origValues=null;
		
	}


	public boolean isNotToAdd() {
		return notToAdd;
	}


	public void setNotToAdd(boolean notToAdd) {
		this.notToAdd = notToAdd;
	}


	public boolean isModifying() {
		return modifying;
	}


	public void setModifying(boolean modifying) {
		this.modifying = modifying;
	}


	public ArrayList<String> getOrigValues() {
		return origValues;
	}

	public void actionPerformed(ActionEvent e) {
		
		if(e.getActionCommand()=="addMacro"){
			
			addParameter();
			
			
		}else if(e.getActionCommand().equals("deleteParameters")){
			for(int i=0;i<macros.size();i++){
				model.sendToBiocham("delete_macro("+macros.get(i).getName()+").\n");
			}
			macros.clear();
			panel.removeAll();	
		
			ArrayList cs=new ArrayList(0);
			panel=refreshPanel(cs);			
			cs.clear();
			cs=null;			
			panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));	
			panel.revalidate();
			
			
		}
	}


	public void addParameter() {
		
		setNewAdded(true);
		element=model.getMacros();
		setSavedResponses(-1);
		DialogAddParameter param=new DialogAddParameter(BiochamMainFrame.frame,model.getMacros(),"macro","macro",panel);	
		Object[] newParam=new Object[2];
		newParam=param.getNewParameter();
		String name=(String)newParam[0];
		String value=(String)newParam[1];
		param=null;
		newParam=null;
		
		if(name!=null && value!=null){
			
			int i=indexOf(name);
			if(i<0){    					
				String s="macro("+name+","+value+").\n";	    					
				model.sendToBiocham(s,"macros");
			}else{
				
					Component[] comps=panel.getComponents();	
					for(int k=0;k<comps.length;k++){
										  
						if(comps[k].getName().equals(name) && comps[k] instanceof JFormattedTextField){
							
							((JFormattedTextField)comps[k]).setValue(value);
							setModified(((JFormattedTextField)comps[k]));
							setNotToAdd(true);
							model.sendToBiocham("macro("+name+","+value+").\n","macros");
							macros.remove(i);
							macros.add(i,new Macro(name,value));
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
	
	public void removeLastAdded() {
		resetParameter(lastAdded);
		
	}


	public boolean isModiff() {
		return modiff;
	}


	public void setModiff(boolean modiff) {
		this.modiff = modiff;
	}
}