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
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.Spring;
import javax.swing.SpringLayout;
import javax.swing.SwingUtilities;







public class ParamTableVolumes implements Parameters, ActionListener{

	
	private Vector<Volume> volumes;
	WorkbenchArea biocham;
	JPanel panel;
	BiochamModel model;
	BiochamModelElement element;
	int savedResponses;
	boolean deleting=false;
	boolean notToAdd=false;
	ArrayList<String> origValues;	
	BalloonTip bTip;
	boolean newAdded=false;
	boolean modifying=false;
	String lastAdded="";
	
	VolumesModel volumesModel;
	VolumesView view;
	
	
	
	public VolumesView getView() {
		return view;
	}
	public void setView(VolumesView view) {
		this.view = view;
	}
	public VolumesModel getVolumesModel() {
		return volumesModel;
	}
	public void setVolumesModel(VolumesModel volumesModel) {
		this.volumesModel = volumesModel;
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
	
	public ParamTableVolumes(BiochamModel m,WorkbenchArea workbench, JPanel p){
		
		boolean b=SwingUtilities.isEventDispatchThread();
		
		biocham=workbench;
		model=m;
		panel=p;
		setVolumes(new Vector<Volume>());
		savedResponses=-1;
		origValues=new ArrayList<String>();
		Utils.modern.setBorderThickness(3);
		Utils.modern.enableAntiAliasing(true);		
		
		volumesModel=new VolumesModel(model);
		view=new VolumesView(BiochamMainFrame.frame,volumesModel);
	}

	
	public String getName(int i) {
		return volumes.get(i).getName();
	}


	public String getValue(int i) {
		return volumes.get(i).getValue();
	}


	public int indexOf(String paramName) {
		
		int i=0;
	    while (i<volumes.size() && !getName(i).equals(paramName))
	    	i++;
	    if (i == volumes.size())
	        return -1;
	    return i;
	}


	public void resetParameter(String s) {
		
		int i = indexOf(s);
				
		if (i>=0) {
					
			String ovalue=getOrigValues().get(i);
			JFormattedTextField tf = ((JFormattedTextField) panel.getComponent(4*i+1));
		    tf.setValue(ovalue);
		    tf.setBackground(Color.WHITE);
		    String s1="volume("+s+","+ovalue+").\n";
		    setNotToAdd(true);
			model.sendToBiocham(s1,"volumes");	
			volumes.remove(i);
			volumes.add(i,new Volume(s,ovalue));
			refreshAfterAddingNewParams();
			tf=null;
		}		
		
	}


	public void setModified(Component comp) {	
		comp.setBackground(Utils.modifiedParameterColor);	
		
	}


	public void setValue(ArrayList list) {
		
		
		
		String arg1,arg2;
		arg1=(String)list.get(0);
		arg2=(String)list.get(1);	
		if(!arg2.trim().equals("1")){
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
				Volume volume=new Volume(arg1,arg2);
				getOrigValues().add(arg2);	
				volumesModel.addVolume(arg1, arg2);
				volumes.add(volume); //vector of parameters
				//BiochamMenuBar.refreshMenusContext(model);
			
				volume=null;
				
				int numParam = volumes.size()-1;

				Spring maxSpring = Spring.constant(10),maxSpring2=Spring.constant(10);
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
														lastAdded=name;
														model.sendToBiocham("volume("+name+","+tf.getText()+").\n","volumes");
														volumes.remove(i);
														volumes.add(i,new Volume(name,tf.getText()));	
														refreshAfterAddingNewParams();
														comps=null;
														scrC=null;
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
						setModifying(true);
	    				setSavedResponses(-1);
	    				Component button=(Component) e.getSource();
	    				String name=button.getName();	    				
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
	    			    cs.clear();
	    			    cs=null;
	    			    button=null;
	    			    tf=null;
	    			    panel=null;
	    			    comps=null;
					}				
		    	});
		    	DeleteButton but2=new DeleteButton();
		    	but2.setName(arg1);
		    	but2.addMouseListener(new MouseAdapter(){

					public void mouseClicked(MouseEvent e) {
						Component button=(Component) e.getSource();
	    				String name=button.getName();
	    				int k=indexOf(name);
	    				volumes.remove(k);
	    				//BiochamMenuBar.refreshMenusContext(model);
	    			
	    				getOrigValues().remove(k);
	    				setDeleting(true);
	    				String s="volume("+name+","+1+").\n";	    				
						model.sendToBiocham(s,"volumes");	    				
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
								if(!((JButton)comps[i]).getActionCommand().equals("addVolume")){
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
		          	    		
		    	panel.add(tf);
		    	panel.add(but1);
		    	panel.add(but2); 
		     
		    	layout.putConstraint(SpringLayout.WEST, l, 5,SpringLayout.WEST, panel);
		    	layout.putConstraint(SpringLayout.NORTH, l, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);
		    	layout.putConstraint(SpringLayout.NORTH, tf, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);  
		    	layout.putConstraint(SpringLayout.NORTH, but1, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);         
		    	layout.putConstraint(SpringLayout.WEST, but1, 5,SpringLayout.EAST, tf);
		    	layout.putConstraint(SpringLayout.NORTH, but2, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);
		    	layout.putConstraint(SpringLayout.WEST, but2, LEFT_OFF, SpringLayout.EAST, but1);
		     
		    	
		    	int max=10,temp=0;
		    	for(int j=0;j<volumes.size();j++){
		    		if(panel.getComponent(4*j+1) instanceof JFormattedTextField){
		    			temp=((JFormattedTextField)panel.getComponent(4*j+1)).getColumns();
		    			if(temp>=max){
		    				max=temp;
		    			}
		    		}
		    	}
		    	for(int j=0;j<volumes.size();j++){
		    		if(panel.getComponent(4*j+1) instanceof JFormattedTextField){
		    			((JFormattedTextField)panel.getComponent(4*j+1)).setColumns(max);
		    		}
		    	}			
		     
		     
		    	for (i=0;i<volumes.size();++i){
		    		maxSpring = Spring.max(maxSpring, Spring.sum(layout.getConstraints(panel.getComponent(4*i)).getWidth(),Spring.constant(10)));
		    	}
		    	for (i=0;i<volumes.size();++i){
		    		layout.putConstraint(SpringLayout.WEST, panel.getComponent(4*i+1), maxSpring, SpringLayout.WEST, panel);
		    	}
		    	tf=null;
		    	but1=null;
		    	but2=null;
		    	l=null;
		    	layout=null;
		    	maxSpring=null;
		    	
			}else{
					
					setSavedResponses(0);
					Component[] comps=panel.getComponents();	
					for(int k=0;k<comps.length;k++){
										  
						if(comps[k].getName().equals(arg1) && comps[k] instanceof JFormattedTextField){
							
							((JFormattedTextField)comps[k]).setValue(arg2);
							setModified(((JFormattedTextField)comps[k]));
							((JFormattedTextField)comps[k]).setEditable(false);
							comps=null;
							return;
						}
					}
			}
			refreshAfterAddingNewParams();
			if(isNewAdded()){
				refreshAfterAddingNewParams();
				setNewAdded(false);
			}
			panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));
			panel.revalidate();
		}
		
	}


	public int size() {
		return volumes.size();
	}
	
	public class Volume {
	      
		
		  private String name;
	      private String value;
	    
	      Volume(String s, String k) {
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
		Spring maxSpring = Spring.constant(10);//,maxSpring2=Spring.constant(20);		
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
			
			layout.putConstraint(SpringLayout.WEST, l, 5,SpringLayout.WEST, panel);
	    	layout.putConstraint(SpringLayout.NORTH, l, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);
	    	layout.putConstraint(SpringLayout.NORTH, tf, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel); 
	    	layout.putConstraint(SpringLayout.NORTH, but1, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);         
	    	layout.putConstraint(SpringLayout.WEST, but1, 5,SpringLayout.EAST, tf);
	    	layout.putConstraint(SpringLayout.NORTH, but2, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);
	    	layout.putConstraint(SpringLayout.WEST, but2, LEFT_OFF, SpringLayout.EAST, but1);
	     
	    	
			int max=10;
			String elem_max = null;
	    	for(int j=0;j<volumes.size();j++){
	    		try{
	    			if(panel.getComponent(4*j+1) instanceof JFormattedTextField){
	    				elem_max=((JFormattedTextField)panel.getComponent(4*j+1)).getText();
	    				if(elem_max.length()>=max){
	    					max=elem_max.length();
	    				}
	    			}
	    		}catch(Exception e){}
	    	}
	    	for(int j=0;j<volumes.size();j++){
	    		try{
	    			if(panel.getComponent(4*j+1) instanceof JFormattedTextField){
	    				((JFormattedTextField)panel.getComponent(4*j+1)).setColumns(max-max/4); 
	    			}
	    		}catch(Exception e){}
	    	}     
	       cp=null;
	       l=null;
	       but1=null;
	       but2=null;
	       tf=null;		    
		}		
	    for (int i=0;i<rows;++i){
    		maxSpring = Spring.max(maxSpring, Spring.sum(layout.getConstraints(panel.getComponent(4*i)).getWidth(),Spring.constant(30)));
    	}
	    for (int i=0;i<rows;++i){
    		layout.putConstraint(SpringLayout.WEST, panel.getComponent(4*i+1), maxSpring, SpringLayout.WEST, panel);
    	}	    
	    maxSpring=null;
	       
	    String toolTipText="<html><i>Define a volume of a location.<br> It may depend on parameters or " +
	    		"concentrations of compound.<br> If no value is provided, it's supposed it's equal to " +
	    		"1,<br>which is a volume of the default location.<br>Example: volume( compartment, 1/parameterName )</i></html>";
	    CustomToolTipButton b1=new CustomToolTipButton("Add",toolTipText);
	    b1.setName("Add");
	    b1.setActionCommand("addVolume");
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
	    		                
                refreshAfterAddingNewParams();				    	
		    	
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
			 layout.putConstraint(SpringLayout.NORTH, b2, HEIGHT,SpringLayout.NORTH, panel);
			 layout.putConstraint(SpringLayout.WEST, b1, 10, SpringLayout.WEST, panel);	
			 layout.putConstraint(SpringLayout.WEST, b2, LEFT_OFF, SpringLayout.EAST, b1);	
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

	public void resetSavedResponses() {
		savedResponses=-1;
	}


	public int getSavedResponses() {
		return savedResponses;
	}


	public void setSavedResponses(int savedResponses) {
		this.savedResponses = savedResponses;
	}


	public boolean isDeleting() {
		return deleting;
	}


	public void setDeleting(boolean deleting) {
		this.deleting = deleting;
	}


	public boolean isNotToAdd() {
		return notToAdd;
	}


	public void setNotToAdd(boolean notToAdd) {
		this.notToAdd = notToAdd;
	}


	public void disposeElements() {
		
		volumes.clear();
		setVolumes(null);
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

	public void actionPerformed(ActionEvent e) {
		if(e.getActionCommand()=="addVolume"){
			
			addParameter();
			
		}else if(e.getActionCommand().equals("deleteParameters")){
			deleteParameters();
			
		}
	}
	private void deleteParameters() {
		for(int i=0;i<volumes.size();i++){
			model.sendToBiocham("volume("+volumes.get(i).getName()+","+1+").\n");
		}
		volumes.clear();		
		panel.removeAll();	
		ArrayList cs=new ArrayList(0);
		panel=refreshPanel(cs);
		cs.clear();
		cs=null;			
	
		panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));	
		panel.revalidate();
		
	}
	/**
	 * 
	 */
	public void refreshAfterAddingNewParams() {
		ArrayList<Component> cs=new ArrayList<Component>();
		Component[] comps=panel.getComponents();
		for(int i=0;i<comps.length;i++){
			if((comps[i] instanceof JButton)){
				if(!((JButton)comps[i]).getActionCommand().equals("addVolume")){
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

	public void addParameter() {
		setNewAdded(true);
		setSavedResponses(-1);
		DialogAddParameter param=new DialogAddParameter(BiochamMainFrame.frame,model.getVolumes(),null,null,panel);	
		Object[] newParam=new Object[2];
		newParam=param.getNewParameter();
		String name=(String)newParam[0];
		String value=(String)newParam[1];
		param=null;
		newParam=null;
		
		if(name!=null && value!=null){
			boolean exists=false;
			for(int i=0;i<volumes.size();i++){
				if(volumes.get(i).getName().equals(name) && volumes.get(i).getValue().equals(value)){
					exists=true;
					break;
				}
			}
			if(!exists){    					
				String s="volume("+name+","+value+").\n";
				model.sendToBiocham(s,"volumes");
			}else{
				JOptionPane.showMessageDialog(BiochamMainFrame.frame, "The same volume is already definied.");
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
	public void setVolumes(Vector<Volume> volumes) {
		this.volumes = volumes;
	}
	public Vector<Volume> getVolumes() {
		return volumes;
	}

}
