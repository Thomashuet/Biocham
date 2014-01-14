package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.BiochamModelElement;
import fr.inria.contraintes.biocham.WorkbenchArea;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.customComponents.DeleteButton;
import fr.inria.contraintes.biocham.customComponents.ModifyButton;
import fr.inria.contraintes.biocham.dialogs.DialogAddParameter;
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

public class ParamTableDeclarations implements Parameters, ActionListener{
		
	
	Vector<Declaration> declarations;
	WorkbenchArea biocham;
	JPanel panel;
	BiochamModel model;
	BiochamModelElement element;
	int savedResponses;
	boolean deleting=false;
	ArrayList<String> origValues;
	boolean notToAdd=false;
	BalloonTip bTip;
	boolean newAdded=false;
	boolean modifying=false;
	String lastAdded="";
	
	DeclarationsModel declarationsModel;
	DeclarationsView view;
	
	public DeclarationsView getView() {
		return view;
	}

	public void setView(DeclarationsView view) {
		this.view = view;
	}

	public DeclarationsModel getDeclarationsModel() {
		return declarationsModel;
	}

	public void setDeclarationsModel(DeclarationsModel declarationsModel) {
		this.declarationsModel = declarationsModel;
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
	
	public ParamTableDeclarations(BiochamModel m,WorkbenchArea workbench, JPanel p){
		
		biocham=workbench;
		model=m;
		panel=p;
		declarations=new Vector<Declaration>();
		savedResponses=-1;
		origValues=new ArrayList<String>();
		Utils.modern.setBorderThickness(3);
		Utils.modern.enableAntiAliasing(true);	
		
		declarationsModel=new DeclarationsModel(model);
		view=new DeclarationsView(BiochamMainFrame.frame,declarationsModel);
	}
	
	
	
	public String getName(int i) {
		return declarations.get(i).getName();
	}

	public String getValue(int i) {
		return declarations.get(i).getValue();
		
	}

	public int indexOf(String paramName) {
		 int i=0;
	     while (i<declarations.size() && !getName(i).equals(paramName))
	         i++;
	     if (i == declarations.size())
	         return -1;
	     return i;
	}
	
	public int indexOfValue(String paramName) {
		 int i=0;
	     while (i<declarations.size() && !getValue(i).equals(paramName))
	         i++;
	     if (i == declarations.size())
	         return -1;
	     return i;
	}


	public void resetParameter(String s) {
		
		int i = indexOf(s);
		JFormattedTextField tf = ((JFormattedTextField) panel.getComponent(4*i+1));
		setSavedResponses(-1);	
		 if(isModifying()){
			 String s1="declare "+s+"~{"+getOrigValues().get(i)+"}.\n";
			 setNotToAdd(true);
			 model.sendToBiocham(s1,"declarations");	
			 declarations.add(i,new Declaration(s,getOrigValues().get(i)));
			
		 }else{
			 if(!getValue(i).equals(getOrigValues().get(i))){
			
				if (i>=0) {
								
					String ovalue=getOrigValues().get(i);				
				    tf.setValue(ovalue);
				    tf.setName(s);
				    panel.getComponent(4*i).setName(s);
				    panel.getComponent(4*i+2).setName(s);
				    panel.getComponent(4*i+3).setName(s);
				    tf.setBackground(Color.WHITE);
				    String s1="";
				    if(!isModifying()){
				    	s1="delete_declaration("+s+").\n";
				    }
				    s1+="declare "+s+"~{"+ovalue+"}.\n";
				    setNotToAdd(true);
					model.sendToBiocham(s1,"declarations");	
					declarations.remove(i);
					declarations.add(i,new Declaration(s,ovalue));
					refreshAfterAddingNew();
				}
			 
			 }else{
				 tf.setBackground(Color.WHITE);
			 }
		 }
		 tf=null;
		
	}
	
	public void setModified(Component comp) {		
		comp.setBackground(Utils.modifiedParameterColor);			
	}

	public int size() {
		return declarations.size();
	}

	
	public void setValue(ArrayList list) {
		
		
		String arg1,arg2;
		arg1=(String)list.get(0);
		arg2=(String)list.get(1);	//modifiable.....	
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
		
		if (i<0) { //there can't be a molecule that has more then 1 declarations.....
			

			Declaration declaration=new Declaration(arg1,arg2);			
			declarations.add(declaration); 
			declarationsModel.addDeclaration(arg1, arg2);
			declaration=null;
			
			
			int numParam = declarations.size()-1;
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
			
			Spring maxSpring = Spring.constant(MIDDLE),ms2=Spring.constant(MIDDLE);
			SpringLayout layout = (SpringLayout) panel.getLayout();
			JLabel l = new JLabel(arg1);
			l.setName(arg1);  			
			tf.setName(arg1);
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
								int index=indexOf(name);						
								String phosphoSites=getValue(index);					
								
								if(e.getKeyCode()==e.VK_ENTER){
								    								    
								    String newValue=tf.getText();
								    if(!newValue.equals(phosphoSites)){
								    	
										String so="delete_declaration("+name+").\n";
										so+="declare "+name+"~parts_of({"+newValue+"}).\n";
										lastAdded=name;
										setNotToAdd(true);
										model.sendToBiocham(so,"declarations");		
										refreshAfterAddingNew();
										
								    }else{
								    	JOptionPane.showMessageDialog(BiochamMainFrame.frame, "The molecule "+name+" has already been associated to this phoshorylation sites.");
								    }
								}
							}
						}
					}		
			   }
			});
			

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
    			    JFormattedTextField tf=null;	    			    
    			    for(int i=0;i<comps.length;i++){    			    	
    			    	if(comps[i].getName().equals(name) && comps[i] instanceof JFormattedTextField){
    			    		tf=(JFormattedTextField)comps[i];	
    			    		break;
    			    	}
    			    }
    			    if(tf!=null){
    			    	tf.setEditable(true);
    			    }    			    	
    			    button=null;
    			    panel=null;
    			    comps=null;	    	
				}
	    	});
	    	
	    	DeleteButton but2=new DeleteButton();
	    	but2.setName(arg1);
	    	but2.addMouseListener(new MouseAdapter(){

	    		public void mouseClicked(MouseEvent e){
	    			Component button=(Component) e.getSource();
    				String name=button.getName();
    				int k=indexOf(name);   			
    				int response=JOptionPane.showConfirmDialog(BiochamMainFrame.frame,"This will not delete the rules that use this declaration.\nAre you sure you want to delete this declaration?");	    				
    				if(response==JOptionPane.YES_OPTION){
    					
    					declarations.remove(k);
    				
	    				getOrigValues().remove(k);
	    				String s="delete_declaration("+name+").\n";	    				
						model.sendToBiocham(s,"declarations");	    				
	    				JPanel panel=(JPanel) button.getParent();			
	    			    Component[] comps=panel.getComponents();
	    			    ArrayList cs=new ArrayList();   			    
	    			    for(int i=0;i<comps.length;i++){
	    			    	if(comps[i].getName().equals(name)){
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
								if(!((JButton)comps[i]).getActionCommand().equals("addDeclaration")){
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
    				}
			    
	    		}
				});
	    	
	    		    	
	    	panel.add(l);
	    	panel.add(tf);
	    	panel.add(but1);
	    	panel.add(but2); 	     
	    	layout.putConstraint(SpringLayout.WEST, l, LEFT_OFF, SpringLayout.WEST, panel);
			layout.putConstraint(SpringLayout.NORTH, l, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);
			layout.putConstraint(SpringLayout.NORTH, tf, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);  
		    layout.putConstraint(SpringLayout.NORTH, but1, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);      
			layout.putConstraint(SpringLayout.NORTH, but2, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);
			layout.putConstraint(SpringLayout.WEST, but2, LEFT_OFF,SpringLayout.EAST, but1);  		
			int max=10,temp=0;
	    	for(int j=0;j<declarations.size();j++){
	    		if(panel.getComponent(4*j+1) instanceof JFormattedTextField){
	    			temp=((JFormattedTextField)panel.getComponent(4*j+1)).getColumns();
	    			if(temp>=max){
	    				max=temp;
	    			}
	    		}
	    	}
	    	for(int j=0;j<declarations.size();j++){
	    		if(panel.getComponent(4*j+1) instanceof JFormattedTextField){
	    			((JFormattedTextField)panel.getComponent(4*j+1)).setColumns(max);//.setColumns(max+1);	    
	    		}
	    	}			
	    	for (i=0;i<declarations.size();++i)
	    		maxSpring = Spring.max(maxSpring, Spring.sum(layout.getConstraints(panel.getComponent(4*i)).getWidth(),Spring.constant(10)));
	    	for (i=0;i<declarations.size();++i)
	    		layout.putConstraint(SpringLayout.WEST, panel.getComponent(4*i+1), maxSpring, SpringLayout.WEST, panel);	    	
	    	for (i=0;i<declarations.size();++i)
	    		ms2 = Spring.max(ms2, Spring.sum(layout.getConstraints(panel.getComponent(4*i+1)).getWidth(),maxSpring));
	    	for (i=0;i<declarations.size();++i)
	    		layout.putConstraint(SpringLayout.WEST, panel.getComponent(4*i+2), ms2, SpringLayout.WEST, panel);
	    	
			
		}else{
			Component[] comps=panel.getComponents();	
			for(int k=0;k<comps.length;k++){
								  
				if(comps[k].getName().equals(arg1) && comps[k] instanceof JFormattedTextField){
					
					((JFormattedTextField)comps[k]).setValue(arg2);
					setModified(((JFormattedTextField)comps[k]));
					((JFormattedTextField)comps[k]).setEditable(false);
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
	
	
	
	
	
	public class Declaration {
	      
		  private String name; //
	      private String value;//modifiable

	      Declaration(String s, String k) {
	         
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

	      public void resetName() {	    	 
	      }
	      
	      /**
	       * Returns true or false depending if the current value is different
	       * from the original value.
	       */
	      
	     /* public boolean hasChanged() {
	         return (! value.equals(origName));
	      }*/
	      

	   }






	public Declaration createNewDeclarationObject(String s, String k){
		return new Declaration(s,k);
	}


	synchronized public JPanel refreshPanel(ArrayList cs) {
		

		SpringLayout layout = (SpringLayout) panel.getLayout();
		Spring maxSpring = Spring.constant(MIDDLE), ms2=Spring.constant(MIDDLE);		
		int rows = cs.size()/4;	
		for(int i=0;i<cs.size();i++){
			if(((Component)cs.get(i)).getName().equals("refresh")){
				cs.remove(i);
			}			
		}
		for(int i=0;i<rows;i++){
			 
			JComponent cp=(JComponent) cs.get(4*i);
			JLabel l=(JLabel)cp;			
			JFormattedTextField tf=(JFormattedTextField)cs.get(4*i+1);
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
			layout.putConstraint(SpringLayout.NORTH, but2, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);	
			layout.putConstraint(SpringLayout.WEST, but2, LEFT_OFF,SpringLayout.EAST, but1);		

		}	
		
		int max=10;
		String elem_max = null;
    	for(int j=0;j<declarations.size();j++){
    		try{
    			if(panel.getComponent(4*j+1) instanceof JFormattedTextField){
    				elem_max=((JFormattedTextField)panel.getComponent(4*j+1)).getText();
    				if(elem_max.length()>=max){
    					max=elem_max.length();
    				}
    			}
    		}catch(Exception e){}
    	}
    	for(int j=0;j<declarations.size();j++){
    		try{
    			if(panel.getComponent(4*j+1) instanceof JFormattedTextField){
    				((JFormattedTextField)panel.getComponent(4*j+1)).setColumns(max-max/4); 
    			}
    		}catch(Exception e){}
    	}
		
    	Utils.debugMsg("Declarations.size="+declarations.size());
    	for (int i=0;i<declarations.size();++i)
    		maxSpring = Spring.max(maxSpring, Spring.sum(layout.getConstraints(panel.getComponent(4*i)).getWidth(),Spring.constant(10)));
    	for (int i=0;i<declarations.size();++i)
    		layout.putConstraint(SpringLayout.WEST, panel.getComponent(4*i+1), maxSpring, SpringLayout.WEST, panel);
    	
    	for (int i=0;i<declarations.size();++i)
    		ms2 = Spring.max(ms2, Spring.sum(layout.getConstraints(panel.getComponent(4*i+1)).getWidth(),Spring.sum(maxSpring,Spring.constant(10))));
    	for (int i=0;i<declarations.size();++i)
    		layout.putConstraint(SpringLayout.WEST, panel.getComponent(4*i+2), ms2, SpringLayout.WEST, panel);
    	
    		    
    	String toolTipText="<html><i>Associate to a molecule a set of sites which can be phosphorylated.<br>Example: name=MA where MA represents a molecule name, and<br>&nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp  value=p1,p2,p3, where pi represent phosphorylation sites.</i></html>";
	    CustomToolTipButton b1=new CustomToolTipButton("Add",toolTipText);	
	    b1.setName("Add");
	    b1.setBalloonToolTipVisible(false);
	    b1.setActionCommand("addDeclaration");
	    b1.addActionListener(this);	  
	    JLabel refreshLabel=new JLabel();
	    refreshLabel.setIcon(Icons.icons.get("Refresh3.png"));
	    refreshLabel.setName("refresh");	    
	    refreshLabel.setText("Screen Refresh");
	    refreshLabel.setForeground(Utils.refreshedColor);
	    refreshLabel.setToolTipText("Click to Refresh the Screen");
	    refreshLabel.addMouseListener(new MouseAdapter(){
	    	public void mouseClicked(MouseEvent me) {
	    		
                             
                refreshAfterAddingNew();	
            }

			
	    });
	  
		Component[] comps=panel.getComponents();
		
		if(comps.length>0 && !(comps[0] instanceof JButton)){
			
			 
			 panel.add(b1);
			 panel.add(refreshLabel);			
			 layout.putConstraint(SpringLayout.NORTH, b1,40,SpringLayout.NORTH,comps[comps.length-3]);
	         layout.putConstraint(SpringLayout.WEST, b1, LEFT_OFF, SpringLayout.WEST, panel);
	         layout.putConstraint(SpringLayout.NORTH, refreshLabel, 0,SpringLayout.NORTH, b1);
	         layout.putConstraint(SpringLayout.WEST, refreshLabel, 10, SpringLayout.EAST, b1);
	         
		}else{
			 panel.add(b1);
			 panel.add(refreshLabel);
			 layout.putConstraint(SpringLayout.NORTH, b1, HEIGHT,SpringLayout.NORTH, panel);
			 layout.putConstraint(SpringLayout.WEST, b1, 10, SpringLayout.WEST, panel);
			 layout.putConstraint(SpringLayout.NORTH, refreshLabel, HEIGHT,SpringLayout.NORTH, panel);
			 layout.putConstraint(SpringLayout.WEST, refreshLabel, 10, SpringLayout.EAST, b1);
			 
		}		
	    cs.clear();
	    panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));	
	    panel.validate();
		panel.repaint();			
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
				if(!((JButton)comps[i]).getActionCommand().equals("addDeclaration")){
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


	public boolean isDeleting() {
		return deleting;
	}


	public void setDeleting(boolean deleting) {
		this.deleting = deleting;
	}



	public void disposeElements() {
		
		declarations.clear();
		declarations=null;
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



	public boolean isNotToAdd() {
		return notToAdd;
	}



	public void setNotToAdd(boolean notToAdd) {
		this.notToAdd = notToAdd;
	}

	public void actionPerformed(ActionEvent e) {
		
		if(e.getActionCommand()=="addDeclaration"){
			
			addParameter();				
		}else if(e.getActionCommand().equals("deleteParameters")){
		
			for(int i=0;i<declarations.size();i++){
				model.sendToBiocham("delete_declaration("+declarations.get(i).getName()+").\n");
			}
			declarations.clear();
		
			panel.removeAll();	
			ArrayList cs=new ArrayList(0);
			panel=refreshPanel(cs);
			cs.clear();
			cs=null;
			panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));	
		}
	}



	public void addParameter() {
		
		setNewAdded(true);
		DialogAddParameter param=new DialogAddParameter(BiochamMainFrame.frame,model.getDeclarations(),null,null,panel);
		Object[] newParam=new Object[2];
		newParam=param.getNewParameter();
		String name=(String)newParam[0];
		String value=(String)newParam[1];
		
		if(name!=null && value!=null){
			boolean exists=false;
			for(int i=0;i<declarations.size();i++){
				if(declarations.get(i).getName().equals(name)){
					exists=true;
					break;
				}
			}
			if(!exists){    					
				String s="declare "+name+"~parts_of({"+value+"}).\n";
				model.sendToBiocham(s,"declarations");
			}else{
				JOptionPane.showMessageDialog(BiochamMainFrame.frame, "The declaration for this molecule ("+name+") is already definied.");
			}
		}
	}



	public Vector<Declaration> getDeclarations() {
		return declarations;
	}



	public void adjustModifiedValue(String molecule, String partsof) {
		int index=indexOf(molecule);	
		declarations.remove(index);
		declarations.add(index,new Declaration(molecule, partsof));
	    setNotToAdd(false);
	    Component[] comps=panel.getComponents();
	    for(int i=0;i<comps.length;i++){
	    	if(comps[i].getName().equals(molecule) && comps[i] instanceof JFormattedTextField){
	    		JFormattedTextField tf=(JFormattedTextField)comps[i];
	    		tf.setValue(partsof);	
	    		tf.setName(molecule);
	    		String orig=getOrigValues().get(index);
	    		if(!orig.equals(partsof)){
	    			tf.setBackground(Utils.modifiedParameterColor);
	    		}else{
	    			tf.setBackground(Color.WHITE);	
	    		}	    		
	    		tf.setEditable(false);
	    		break;
	    	}
	    }
	    comps=null;
		
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


}
