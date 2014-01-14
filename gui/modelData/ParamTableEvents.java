package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.BiochamModelElement;
import fr.inria.contraintes.biocham.WorkbenchArea;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.dialogs.DialogAddParameter;
import fr.inria.contraintes.biocham.modelData.EventsModel.Event;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.Utils;
import net.java.balloontip.BalloonTip;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.Spring;
import javax.swing.SpringLayout;




public class ParamTableEvents implements Parameters, ActionListener{
	
	
	
	private Vector<Event> events;
	WorkbenchArea biocham;
	JPanel panel;
	BiochamModel model;
	BiochamModelElement element;
	int savedResponses;
	boolean modifying=false;
	static ArrayList<String> origCondition,origName,origKinetics;
	BalloonTip bt1,bt2,bt3;
	boolean newAdded=false;
	JFormattedTextField lastAddedC=null;
	String lastAdded="";
	boolean modiff=false;

	EventsModel eventsModel;
	EventsView view;
	
	
	public EventsView getView() {
		return view;
	}
	public void setView(EventsView view) {
		this.view = view;
	}
	public EventsModel getEventsModel() {
		return eventsModel;
	}
	public void setEventsModel(EventsModel eventsModel) {
		this.eventsModel = eventsModel;
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
	
	public ParamTableEvents(BiochamModel m,WorkbenchArea workbench, JPanel p){
		biocham=workbench;
		model=m;
		panel=p;
		setEvents(new Vector<Event>());
		savedResponses=-1;
		element=model.getEvents();		
		origCondition=new ArrayList<String>();
		origName=new ArrayList<String>();
		origKinetics=new ArrayList<String>();
		Utils.modern.setBorderThickness(3);
		Utils.modern.enableAntiAliasing(true);
		
		eventsModel=new EventsModel(model);
		view=new EventsView(BiochamMainFrame.frame,eventsModel);
	}
	
	
	
	public String getName(int i) {
		return null;//getEvents().get(i).getName();
	}
	String getEventCondition(int i){
		if(getEvents().get(i)!=null){
			return getEvents().get(i).getCondition();
		}else return null;
	}	
	String getEventKinetics(int i){
	    return getEvents().get(i).getKinetics();
	}
	 
	int checkExistence(String event, JPanel p){
	
		Component[] comps=p.getComponents();
		for(int i=0;i<comps.length;i++){
			if(comps[i] instanceof JFormattedTextField){				
				if(((JFormattedTextField)comps[i]).getName().equals(event)){					
					return (i-1)/8;
				}	
			}
		}
		return -1;
	}
	/*int checkPrimExistence(String cond,String name){
		
		int p = 0;     	
    	String nm;
    	while(p<getEvents().size()){
    		p=indexOfEventCondition(cond,p);
    		if(p==-1){return -1;}else{
	    		nm=getEvents().get(p).getName();    		
	    		if(nm.equals(name)){
	    			return p;
	    		}
    		}
    	}
    	return -1;
	}*/
	
	public String getValue(int i) {
		/*int indexParam=model.getParameters().getParamTable().indexOf(events.get(i).getName());
		if(indexParam!=-1){
			return model.getParameters().getParamTable().getValue(indexParam);
		}else{
			
			int response=JOptionPane.showConfirmDialog(BiochamMainFrame.frame, "The parameter "+events.get(i).getName()+" is uknown. Do you want to define it?");
			if(response==JOptionPane.YES_OPTION){
				DialogAddParameter dialog=new DialogAddParameter(BiochamMainFrame.frame,model.getParameters(),events.get(i).getName(),null);
				Object[] newParam=new Object[2];
				newParam=dialog.getNewParameter();
				//ParamTableParameters paramTableParameters=(ParamTableParameters)model.getParameters().getParamTable();
				//JOptionPane.showInputDialog(BiochamMainFrame.frame, "Type the value for the new parameter");
				String cmd="parameter("+events.get(i).getName()+","+newParam[1].toString()+").\n";
				model.sendToBiocham(cmd);
				return newParam[1].toString();
			}else {
				return null;
			}*/
			return null;
		//}
	}
	
	/*public int indexOf(String paramName) {
		
		int i=0;
        while (i<getEvents().size() && !getName(i).equals(paramName))
           i++;
        if (i == getEvents().size())
           return -1;
        return i;
        
	}	
	int indexOfEventCondition(String s) {
		
		int i=0;
	    while (i<getEvents().size() && !getEventCondition(i).equals(s))
	       i++;
	    if (i == getEvents().size())
	        return -1;
	    return i;
	}	
	int indexOfEventKinetics(String s) {
	
		int i=0;
	    while (i<getEvents().size() && !getEventKinetics(i).equals(s))
	           i++;
	    if (i == getEvents().size())
	           return -1;
	    return i;
	}
	public int indexOf(String paramName,int p) {
		
		int i;
		if(p==0){
			i=0;
		}else{
			i=p+1;
		}
        while (i<getEvents().size() && !getName(i).equals(paramName)){
           i++;
        }
        if (i == getEvents().size())
           return -1;
        return i;
        
	}	
	int indexOfEventCondition(String s,int p) {
		
		int i;
		if(p==0){
			i=0;
		}else{
			i=p+1;
		}
	    while (i<getEvents().size() && !getEventCondition(i).equals(s))
	       i++;
	    if (i == getEvents().size())
	        return -1;
	    return i;
	}	
	int indexOfEventKinetics(String s,int p) {
	
		int i;
		if(p==0){
			i=0;
		}else{
			i=p+1;
		}
	    while (i<getEvents().size() && !getEventKinetics(i).equals(s))
	           i++;
	    if (i == getEvents().size())
	           return -1;
	    return i;
	}
	
	*/
	
	public void setValue(ArrayList list) {
		
		/*String condition=(String)list.get(0);
		String name=(String)list.get(1);
		String kinetics=(String)list.get(2);    
//		eventsModel.addEvent(eventsModel.new Event(null,condition,name,kinetics));*/
       // JFormattedTextField tf1,tf2,tf3 = null;
     /*  // int i=checkPrimExistence(condition,name);
              
        // if there is not yet such an event       	          
    	  
  	  	
    	Component[] comps=panel.getComponents();
	        for(int l=0;l<comps.length;l++){  	        	
	        	
			if(comps[l].getName().equals("Add")){				
        		panel.remove(comps[l]);
			}
	        }
	        comps=null;
	          	   
	        
	        Event event=new Event(condition,name,kinetics);
	        getEvents().add(event); 
	        ArrayList l11=new ArrayList();
	        l11.add(name);
	        ArrayList l22=new ArrayList();
        l22.add(kinetics);
	        eventsModel.addEvent(eventsModel.new Event(condition,name,kinetics));
	        updateObserver();
	        event=null;
        
	        int numParam = getEvents().size()-1;	// THE INDEX FOR THE NEW EVENT.....   
	        
	        getOrigCondition().add(numParam,condition);
	        getOrigName().add(numParam,name);
	        getOrigKinetics().add(numParam,kinetics);
	        
	        
	        SpringLayout layout = (SpringLayout) panel.getLayout();
	        JLabel l1,l2,l3;
	        l1= new JLabel("condition:");
	        l1.setName(condition);
	        tf1 = new JFormattedTextField(); 
	       
	        tf1.setColumns(10);
	        tf1.setValue(condition);
	        tf1.setHorizontalAlignment(JTextField.LEFT);
	        tf1.setEditable(false);
	        tf1.addMouseListener(new MouseAdapter(){
	        	public void mousePressed(MouseEvent e) {	   
	        		if (e.getClickCount() == 2) {
	        			resetParameter(e.getComponent());		   		  
	        		}
	        	}
	        });
      	tf1.addKeyListener(new KeyAdapter(){
			
			public void keyPressed(KeyEvent e) {
				   
				Object src=(Component) e.getSource();
				
				if(src instanceof Component){
					Component scrC=(Component)src;						
					if(scrC instanceof JFormattedTextField){
						
						if(((JFormattedTextField) scrC).isEditable()){
							
							JFormattedTextField tf=(JFormattedTextField) scrC;
							String oldName=tf.getName();// THE OLD VALUE...because the component has its name...
							
							if(e.getKeyCode()==e.VK_ENTER){
								
								
								setModifying(true);																	    
							    String newCond=tf.getText(),newParam = null,newKine = null;							    
							    String param,kine,cond;
							    scrC=null;
			    				int ind1=oldName.indexOf('-');	 
			    				String temp=oldName.substring(ind1+1);
			    				int ind2=temp.indexOf('-');				    				
			    				cond=oldName.substring(0,ind1);
			    				param=temp.substring(0,ind2);	    				
			    				kine=temp.substring(ind2+1);				    				
			    								    				
			    				int row=checkExistence(oldName,panel);
			    				lastAdded=oldName;
			    				lastAddedC=tf;
							    Component[] comps=panel.getComponents();							    
							    JFormattedTextField tf1=(JFormattedTextField)comps[8*row+3];//param
				    			JFormattedTextField tf2=(JFormattedTextField)comps[8*row+5];//kine
				    			JLabel l1=(JLabel)comps[8*row];//cond
				    			JLabel l2=(JLabel)comps[8*row+2];//param
				    			JLabel l3=(JLabel)comps[8*row+4];//kine
				    			JLabel b1=(JLabel)comps[8*row+6];
				    			JLabel b2=(JLabel)comps[8*row+7];
							    
				    			newParam=tf1.getText();
				    			newKine=tf2.getText();					    			
				    			String newName=newCond+"-"+newParam+"-"+newKine;
				    			
				    			if(checkExistence(newName,panel)<0){//the new entered value doesn't exist yet....
				    				tf.setName(newName);
				    				tf1.setName(newName);
				    				tf2.setName(newName);
				    				tf.setEditable(false);
				    				setModified(tf);
				    				setModified(tf1);
				    				setModified(tf2);
				    				tf.setValue(newCond);
				    				tf1.setValue(newParam);
				    				tf1.setEditable(false);
				    				tf2.setValue(newKine);
				    				tf2.setEditable(false);
				    				l1.setName(newCond);
				    				l2.setName(newParam);
				    				l3.setName(newKine);
				    				b1.setName(newName);
				    				b2.setName(newName);
				    				getEvents().remove(row);
				    				getEvents().add(row,new Event(newCond,newParam,newKine));
				    				String cmd="delete_event("+cond+","+param+","+kine+").\n";
				    				cmd+="add_event("+newCond+","+newParam+","+newKine+").\n";
				    				
							    	model.sendToBiocham(cmd,"events");
				    				
				    			}else{// the new entered value exists deja....
				    				JOptionPane.showMessageDialog(BiochamMainFrame.frame, "This event is already definied.");
				    			}
				    			comps=null;
							    tf=null;
							    tf1=null;tf2=null;l1=null;l2=null;l3=null;b1=null;b2=null;
							}
						}
					}						
				}		
		   }
		});//(model); //On click ENTER call the action that modifies the value
      	l1.setLabelFor(tf1);
      	tf1.setName(condition+"-"+name+"-"+kinetics);
      	tf1.setBackground(Color.white);
      	panel.add(l1);
      	panel.add(tf1);
      
      	l2 = new JLabel("name:");
      	l2.setName(name);          
      	tf2 = new JFormattedTextField();      
      	tf2.setColumns(10);
      	tf2.setValue(name);
      	tf2.setHorizontalAlignment(JTextField.LEFT);
      	tf2.setEditable(false);
      	tf2.addMouseListener(new MouseAdapter(){
	        	public void mousePressed(MouseEvent e) {	   
	        		if (e.getClickCount() == 2) {
	        			resetParameter(e.getComponent());		   		  
	        		}
	        	}
	        });
      	tf2.addKeyListener(new KeyAdapter(){
			
			public void keyPressed(KeyEvent e) {
				   
				Object src=(Component) e.getSource();
				
				if(src instanceof Component){
					Component scrC=(Component)src;
					
					if(scrC instanceof JFormattedTextField){
						
						if(((JFormattedTextField) scrC).isEditable()){
							
							JFormattedTextField tf=(JFormattedTextField) scrC;
							String oldName=tf.getName();// THE OLD VALUE...because the component has its name...
							scrC=null;
							
							if(e.getKeyCode()==e.VK_ENTER){
								
								
								setModifying(true);																	    
							    String newParam=tf.getText(),newCond,newKine;
							    String param,cond,kine;
							    
							    int ind1=oldName.indexOf('-');	 
			    				String temp=oldName.substring(ind1+1);
			    				int ind2=temp.indexOf('-');				    				
			    				cond=oldName.substring(0,ind1);
			    				param=temp.substring(0,ind2);	    				
			    				kine=temp.substring(ind2+1);				    				
			    			 				
							    
			    				int row=checkExistence(oldName,panel);
			    				lastAdded=oldName;
			    				lastAddedC=tf;
							    Component[] comps=panel.getComponents();							    
							    JFormattedTextField tf1=(JFormattedTextField)comps[8*row+1];//cond
				    			JFormattedTextField tf2=(JFormattedTextField)comps[8*row+5];//kine
				    			JLabel l1=(JLabel)comps[8*row];//cond
				    			JLabel l2=(JLabel)comps[8*row+2];//param
				    			JLabel l3=(JLabel)comps[8*row+4];//kine
				    			JLabel b1=(JLabel)comps[8*row+6];
				    			JLabel b2=(JLabel)comps[8*row+7];
							    
				    			newCond=tf1.getText();
				    			newKine=tf2.getText();					    			
				    			String newName=newCond+"-"+newParam+"-"+newKine;
				    			
				    			if(checkExistence(newName,panel)<0){//the new entered value doesn't exist yet....
				    				tf.setName(newName);
				    				tf1.setName(newName);
				    				tf2.setName(newName);
				    				tf.setEditable(false);
				    				setModified(tf);
				    				setModified(tf1);
				    				setModified(tf2);
				    				tf.setValue(newParam);
				    				tf1.setValue(newCond);
				    				tf1.setEditable(false);
				    				tf2.setValue(newKine);
				    				tf2.setEditable(false);
				    				l1.setName(newCond);
				    				l2.setName(newParam);
				    				l3.setName(newKine);
				    				b1.setName(newName);
				    				b2.setName(newName);
				    				getEvents().remove(row);
				    				
				    				getEvents().add(row,new Event(newCond,newParam,newKine));
				    				String cmd="delete_event("+cond+","+param+","+kine+").\n";
				    				cmd+="add_event("+newCond+","+newParam+","+newKine+").\n";
							    	model.sendToBiocham(cmd,"events");
				    				
				    			}else{// the new entered value exists deja....
				    				JOptionPane.showMessageDialog(BiochamMainFrame.frame, "This event is already definied.");
				    			}
				    			comps=null;
							    tf=null;tf1=null;tf2=null;l1=null;l2=null;l3=null;b1=null;b2=null;
							}
						}							
					}						
				}		
			}
		});//(model); //On click ENTER call the action that modifies the value
      	l2.setLabelFor(tf2);
      	tf2.setName(condition+"-"+name+"-"+kinetics);
      	tf2.setBackground(Color.white);
      	panel.add(l2);
      	panel.add(tf2);
      
      	l3 = new JLabel("kinetics:");
      	l3.setName(kinetics);        
        tf3 = new JFormattedTextField();  
        bt1=new BalloonTip(tf3,"Don't forget to press ENTER to apply the modification.",Utils.modern,BalloonTip.Orientation.RIGHT_BELOW,BalloonTip.AttachLocation.SOUTHEAST,20,10,false);
	        bt1.setText("Don't forget to press ENTER to apply the modification.");
	        bt1.setIcon(Icons.icons.get("flag_blue.png"));
	        bt1.setIconTextGap(10);	
	        bt1.enableClickToHide(true);
	        bt1.enableClickToClose(true);		
	        tf3.addPropertyChangeListener(new PropertyChangeListener(){

			public void propertyChange(PropertyChangeEvent evt) {
				JFormattedTextField tf=(JFormattedTextField)evt.getSource();
				if(tf.isEditable()){
					bt1.setVisible(true);
				}else{
					bt1.setVisible(false);
				}
				
			}});
        tf3.setColumns(10);
        tf3.setValue(kinetics);
        tf3.setHorizontalAlignment(JTextField.LEFT);
        tf3.setEditable(false);
        tf3.addMouseListener(new MouseAdapter(){
	        	public void mousePressed(MouseEvent e) {	   
	        		if (e.getClickCount() == 2) {
	        			resetParameter(e.getComponent());		   		  
	        		}
	        	}
	        });
        tf3.addKeyListener(new KeyAdapter(){
			
			public void keyPressed(KeyEvent e) {
				   
				Object src=(Component) e.getSource();
				
				if(src instanceof Component){
					Component scrC=(Component)src;
					
					if(scrC instanceof JFormattedTextField){
						
						if(((JFormattedTextField) scrC).isEditable()){
							
							JFormattedTextField tf=(JFormattedTextField) scrC;
							String oldName=tf.getName();// THE OLD VALUE...because the component has its name...
							scrC=null;
							
							if(e.getKeyCode()==e.VK_ENTER){
								
								
								setModifying(true);													    
							    String newKine=tf.getText(),newCond,newParam;								   
							    String cond,param,kine;
							    
							    int ind1=oldName.indexOf('-');	 
			    				String temp=oldName.substring(ind1+1);
			    				int ind2=temp.indexOf('-');				    				
			    				cond=oldName.substring(0,ind1);
			    				param=temp.substring(0,ind2);	    				
			    				kine=temp.substring(ind2+1);				    				
			    				
							   
			    				int row=checkExistence(oldName,panel);
			    				lastAdded=oldName;
			    				lastAddedC=tf;
							    Component[] comps=panel.getComponents();							    
							    JFormattedTextField tf1=(JFormattedTextField)comps[8*row+1];//cond
				    			JFormattedTextField tf2=(JFormattedTextField)comps[8*row+3];//param
				    			JLabel l1=(JLabel)comps[8*row];//cond
				    			JLabel l2=(JLabel)comps[8*row+2];//param
				    			JLabel l3=(JLabel)comps[8*row+4];//kine
				    			JLabel b1=(JLabel)comps[8*row+6];
				    			JLabel b2=(JLabel)comps[8*row+7];
							    
				    			newCond=tf1.getText();
				    			newParam=tf2.getText();					    			
				    			String newName=newCond+"-"+newParam+"-"+newKine;
				    			
				    			if(checkExistence(newName,panel)<0){//the new entered value doesn't exist yet....
				    				tf.setName(newName);
				    				tf1.setName(newName);
				    				tf2.setName(newName);
				    				tf.setEditable(false);
				    				setModified(tf);
				    				setModified(tf1);
				    				setModified(tf2);
				    				tf.setValue(newKine);
				    				tf1.setValue(newCond);
				    				tf1.setEditable(false);
				    				tf2.setValue(newParam);
				    				tf2.setEditable(false);
				    				l1.setName(newCond);
				    				l2.setName(newParam);
				    				l3.setName(newKine);
				    				b1.setName(newName);
				    				b2.setName(newName);
				    				getEvents().remove(row);
				    				
				    				getEvents().add(row,new Event(newCond,newParam,newKine));
				    				String cmd="delete_event("+cond+","+param+","+kine+").\n";
				    				cmd+="add_event("+newCond+","+newParam+","+newKine+").\n";
							    	model.sendToBiocham(cmd,"events");
				    				
				    			}else{// the new entered value exists deja....
				    				JOptionPane.showMessageDialog(BiochamMainFrame.frame, "This event is already definied.");
				    			}
				    			comps=null;
							    tf=null;tf1=null;tf2=null;l1=null;l2=null;l3=null;b1=null;b2=null;
							}
						}
					}
				}												
			}		
        
		});//(model); //On click ENTER call the action that modifies the value
        l3.setLabelFor(tf3);
        tf3.setName(condition+"-"+name+"-"+kinetics);
        tf3.setBackground(Color.white);
        panel.add(l3);
        panel.add(tf3);
      
        ModifyButton but1=new ModifyButton();	
        but1.setName(condition+"-"+name+"-"+kinetics);
    	but1.addMouseListener(new MouseAdapter(){

			public void mouseClicked(MouseEvent e) {
				setModiff(true);
				setSavedResponses(-1);
				Component button=(Component) e.getSource();
				String name=button.getName();
				button=null;	    			    		    				
				int row=checkExistence(name,panel);
				Component[] comps=panel.getComponents();	    				
				JFormattedTextField tf1= (JFormattedTextField) comps[8*row+1];
			    tf1.setEditable(true);
			    JFormattedTextField tf2= (JFormattedTextField) comps[8*row+3];
			    tf2.setEditable(true);
			    JFormattedTextField tf3= (JFormattedTextField)comps[8*row+5];
			    tf3.setEditable(true);	    				
			    comps=null;
			    panel.getParent().validate();
			    panel.getParent().repaint();
			    tf1=null;
			    tf2=null;
			    tf3=null;		    
			}
    	});
    	
    	DeleteButton but2=new DeleteButton();
    	but2.setName(condition+"-"+name+"-"+kinetics);
    	but2.addMouseListener(new MouseAdapter(){

    		public void mouseClicked(MouseEvent e){
    			Component button=(Component) e.getSource();
				String name=button.getName();
				button=null;
				String paramName=null,condName = null,kineName=null;
				
				int ind1=name.indexOf('-');	 
				String temp=name.substring(ind1+1);
				int ind2=temp.indexOf('-');
				
				condName=name.substring(0,ind1);
				paramName=temp.substring(0,ind2);	    				
				kineName=temp.substring(ind2+1);
			    			    				
				int p=checkExistence(name,panel);
		    	getEvents().remove(p);
		    	updateObserver();
		    	getOrigCondition().remove(p);
		    	getOrigName().remove(p);
		    	getOrigKinetics().remove(p);
		    	
				
			    Component[] comps=panel.getComponents();
			   
				JLabel l1=(JLabel)comps[8*p];
				JLabel l2=(JLabel)comps[8*p+2];
				JLabel l3=(JLabel)comps[8*p+4];
				panel.remove(l1);
				panel.remove(l2);
				panel.remove(l3);	    				
			    JFormattedTextField tf1= (JFormattedTextField) comps[8*p+1];	    			    
			    JFormattedTextField tf2= (JFormattedTextField) comps[8*p+3];    			    
			    JFormattedTextField tf3= (JFormattedTextField)comps[8*p+5];
			    panel.remove(tf1);
			    panel.remove(tf2);
			    panel.remove(tf3);	    
			    JLabel but1=(JLabel)comps[8*p+6];
			    JLabel but2=(JLabel)comps[8*p+7];
			    panel.remove(but1);	 
			    panel.remove(but2);	 	    			    
			    tf1=null;
			    tf2=null;
			    tf3=null;
			    l1=null;
			    l2=null;
			    l3=null;
			    but1=null;
			    but2=null;
			    comps=null;
			    String cmd="delete_event("+condName+",["+paramName+"],["+kineName+"]).\n";
		    	model.sendToBiocham(cmd,"events");
		    	
		    	comps=panel.getComponents();
		    	ArrayList cs=new ArrayList();
			    for(int i=0;i<comps.length;i++){	
			    	if((comps[i] instanceof JButton)){
						if(!((JButton)comps[i]).getActionCommand().equals("addEvent")){
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
			    comps=null;
			    panel.removeAll();
			    refreshPanel(cs);
			    panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));	
			    cs.clear();
			    cs=null;
			    panel.getParent().validate();
				panel.getParent().repaint(); 
    		}
			});
    		    	            
        panel.add(but1);
        panel.add(but2); 
        
        layout.putConstraint(SpringLayout.WEST,  l1, LEFT_OFF, SpringLayout.WEST, panel);
        layout.putConstraint(SpringLayout.NORTH, l1, TOP_OFF+HEIGHT*events.size(),SpringLayout.NORTH, panel);
      
        layout.putConstraint(SpringLayout.WEST,  tf1, LEFT_OFF,SpringLayout.EAST, l1);
        layout.putConstraint(SpringLayout.NORTH, tf1, TOP_OFF+HEIGHT*events.size(),SpringLayout.NORTH, panel);
      
        layout.putConstraint(SpringLayout.WEST,  l2, 15,SpringLayout.EAST, tf1);
        layout.putConstraint(SpringLayout.NORTH, l2, TOP_OFF+HEIGHT*events.size(),SpringLayout.NORTH, panel);
      
        layout.putConstraint(SpringLayout.WEST,  tf2, LEFT_OFF,SpringLayout.EAST, l2);
        layout.putConstraint(SpringLayout.NORTH, tf2, TOP_OFF+HEIGHT*events.size(),SpringLayout.NORTH, panel);
      
        layout.putConstraint(SpringLayout.WEST,  l3, 15,SpringLayout.EAST, tf2);
        layout.putConstraint(SpringLayout.NORTH, l3, TOP_OFF+HEIGHT*events.size(),SpringLayout.NORTH, panel);
      
        layout.putConstraint(SpringLayout.WEST,  tf3, LEFT_OFF,SpringLayout.EAST, l3);
        layout.putConstraint(SpringLayout.NORTH, tf3, TOP_OFF+HEIGHT*events.size(),SpringLayout.NORTH, panel);
        
        layout.putConstraint(SpringLayout.NORTH, but1, TOP_OFF+HEIGHT*events.size(),SpringLayout.NORTH, panel);         
        layout.putConstraint(SpringLayout.WEST,  but1, 10,SpringLayout.EAST, tf3);
        
        layout.putConstraint(SpringLayout.NORTH, but2, TOP_OFF+HEIGHT*events.size(),SpringLayout.NORTH, panel);
        layout.putConstraint(SpringLayout.WEST,  but2, 10,SpringLayout.EAST, but1);
                      
        tf1=null;tf2=null;l1=null;l2=null;l3=null;but1=null;but2=null;layout=null;
      
    
        if (i<0) {
        	
        }else{
        	

        	int k=checkExistence(condition+"-"+name+"-"+kinetics,panel);
        	if(k<0){ //this kinetics is new....
        	
		    	int response=JOptionPane.showConfirmDialog(BiochamMainFrame.frame,"\nThe event for "+condition+" for parameter "+name+" is already definied to kinetics "+getEvents().get(i).getKinetics()+". Do you want to change its kinetics to "+kinetics+"?");
		    	if(response==JOptionPane.YES_OPTION){
		    		
		    				    	
			    	Component[] comps=panel.getComponents();				    
				    String newName=condition+"-"+name+"-"+kinetics;
				    
				    JFormattedTextField t1= (JFormattedTextField) comps[8*i+1];	  
				    t1.setName(newName);
				    setModified(t1);
				    
				    JFormattedTextField t2= (JFormattedTextField) comps[8*i+3];
				    t2.setName(newName);
				    setModified(t2);
				    
				    JFormattedTextField t3= (JFormattedTextField) comps[8*i+5];
				    t3.setName(newName);
				    t3.setText(kinetics);
				    setModified(t3);
				    
				    JLabel l=(JLabel)comps[8*i+4];
				    l.setName(kinetics);
				    
				    JLabel but1=(JLabel)comps[8*i+6];
				    but1.setName(newName);
				    
				    JLabel but2=(JLabel)comps[8*i+7];
				    but2.setName(newName);
				    
			    	getEvents().remove(i);
					getEvents().add(i, new Event(condition,name,kinetics));
					t1=null;
					t2=null;
					t3=null;
					l=null;
					but1=null;
					but2=null;					
					comps=null;
					
		    	}else{
		    		
		    	
		    		setModifying(true); 
		    		String cmd="delete_event("+condition+",["+name+"],["+kinetics+"]).\n";
		    		cmd+="add_event("+condition+",["+name+"],["+getEvents().get(i).getKinetics()+"]).\n";
			    	model.sendToBiocham(cmd,"events");
			    	
			    	Component[] comps=panel.getComponents();			    				    
				    JFormattedTextField t1= (JFormattedTextField) comps[8*i+5];	  
				    t1.setValue(getEvents().get(i).getKinetics());
				    t1.setEditable(false);			  
					t1=null;
					comps=null;
		    	}
        	}else{// The kinetics is not new...
        		JOptionPane.showMessageDialog(BiochamMainFrame.frame, "The event ("+condition+","+name+","+kinetics+") already exists.");
        	}
        }
        
        refreshAfterAddingNew();
    	if(isNewAdded()){
			refreshAfterAddingNew();
			setNewAdded(false);
		}
    	panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));	
    	panel.revalidate();*/
	}

/*	public void resetParamAfterModification(String s,JFormattedTextField tf){
		
		setModifying(true);		
		String paramName=null,condName = null,kineName=null;		
		int ind1=s.indexOf('-');	 
		String temp=s.substring(ind1+1);
		int ind2=temp.indexOf('-');
		
		condName=s.substring(0,ind1);
		paramName=temp.substring(0,ind2);	    				
		kineName=temp.substring(ind2+1);
		int p=checkExistence(tf.getName(),panel);
		String value=tf.getText();
		String name1=tf.getName();
		String paramName1=null,condName1 = null,kineName1=null;		
		ind1=name1.indexOf('-');	 
		temp=name1.substring(ind1+1);
		ind2=temp.lastIndexOf('-');
		
		condName1=name1.substring(0,ind1);
		paramName1=temp.substring(0,ind2);	    				
		kineName1=temp.substring(ind2+1);
		
		if(value.equals(condName1)){
	    	
    		JLabel l=(JLabel)panel.getComponent(8*p);
    		String ovalue=getOrigCondition().get(p);
			tf.setValue(ovalue);			
			tf.setBackground(Color.WHITE);
			
			String newName=ovalue+"-"+paramName+"-"+kineName;
			Component[] comps=panel.getComponents();
			comps[8*p+1].setName(newName);
			comps[8*p+3].setName(newName);
			comps[8*p+5].setName(newName);
			comps[8*p+6].setName(newName);
			comps[8*p+7].setName(newName);
						
			comps=null;			
			tf.setName(newName);//i do it again....
			l.setName(ovalue);
			
			String cmd="";//delete_event("+condName+",["+paramName+"],["+kineName+"]).\n";
	    	cmd+="add_event("+ovalue+",["+paramName+"],["+kineName+"]).\n";
	    	model.sendToBiocham(cmd,"events");	    	
	    	getEvents().remove(p);
	    	getEvents().add(p,new Event(ovalue,paramName,kineName));
	    	l=null;
	    	tf=null;
	    	
			
    		
    	}else if(value.equals(paramName1)){
    		    		
    		JLabel l=(JLabel)panel.getComponent(8*p+2);
    		String ovalue=getOrigName().get(p);
			tf.setValue(ovalue);
			tf.setBackground(Color.WHITE);			
			String newName=condName+"-"+ovalue+"-"+kineName;
			Component[] comps=panel.getComponents();
			comps[8*p+1].setName(newName);
			comps[8*p+3].setName(newName);
			comps[8*p+5].setName(newName);
			comps[8*p+6].setName(newName);
			comps[8*p+7].setName(newName);
			tf.setName(newName);
			l.setName(ovalue);			
			//String cmd="delete_event("+condName1+",["+paramName1+"],["+kineName+"]).\n";
			String cmd="add_event("+condName+",["+ovalue+"],["+kineName+"]).\n";
	    	model.sendToBiocham(cmd,"events");	    	
	    	getEvents().remove(p);
	    	getEvents().add(p,new Event(condName,ovalue,kineName));
	    	l=null;
	    	tf=null;
	    	comps=null;
    		
    	}else{
    		
    		JLabel l=(JLabel)panel.getComponent(8*p+4);
    		String ovalue=getOrigKinetics().get(p);
			tf.setValue(ovalue);
			tf.setBackground(Color.WHITE);			
			String newName=condName+"-"+paramName+"-"+ovalue;
			Component[] comps=panel.getComponents();
			comps[8*p+1].setName(newName);
			comps[8*p+3].setName(newName);
			comps[8*p+5].setName(newName);
			comps[8*p+6].setName(newName);
			comps[8*p+7].setName(newName);
			tf.setName(newName);
			l.setName(ovalue);			
			String cmd="";//delete_event("+condName+",["+paramName+"],["+kineName+"]).\n";
	    	cmd+="add_event("+condName+",["+paramName+"],["+ovalue+"]).\n";
	    	model.sendToBiocham(cmd,"events");	    	
	    	getEvents().remove(p);
	    	getEvents().add(p,new Event(condName,paramName,ovalue));
    		
	    	l=null;
	    	tf=null;
	    	comps=null;
    	}
		
	}
	public void resetParameter(Component comp) {
		
		
		setModifying(true);		
		String s=comp.getName();
		JFormattedTextField tf=(JFormattedTextField)comp;
		String value=tf.getText();
		
		String paramName=null,condName = null,kineName=null;		
		int ind1=s.indexOf('-');	 
		String temp=s.substring(ind1+1);
		int ind2=temp.indexOf('-');
		
		condName=s.substring(0,ind1);
		paramName=temp.substring(0,ind2);	    				
		kineName=temp.substring(ind2+1);
						
		int p=checkExistence(s,panel);
    	
    	
    	if(value.equals(condName)){
    	    	
    		JLabel l=(JLabel)panel.getComponent(8*p);
    		String ovalue=getOrigCondition().get(p);
			tf.setValue(ovalue);			
			tf.setBackground(Color.WHITE);
			
			String newName=ovalue+"-"+paramName+"-"+kineName;
			Component[] comps=panel.getComponents();
			comps[8*p+1].setName(newName);
			comps[8*p+3].setName(newName);
			comps[8*p+5].setName(newName);
			comps[8*p+6].setName(newName);
			comps[8*p+7].setName(newName);
						
			comps=null;			
			tf.setName(newName);//i do it again....
			l.setName(ovalue);
			
			String cmd="delete_event("+condName+",["+paramName+"],["+kineName+"]).\n";
	    	cmd+="add_event("+ovalue+",["+paramName+"],["+kineName+"]).\n";
	    	model.sendToBiocham(cmd,"events");	    	
	    	getEvents().remove(p);
	    	getEvents().add(p,new Event(ovalue,paramName,kineName));
	    	l=null;
	    	tf=null;
	    	
			
    		
    	}else if(value.equals(paramName)){
    		    		
    		JLabel l=(JLabel)panel.getComponent(8*p+2);
    		String ovalue=getOrigName().get(p);
			tf.setValue(ovalue);
			tf.setBackground(Color.WHITE);			
			String newName=condName+"-"+ovalue+"-"+kineName;
			Component[] comps=panel.getComponents();
			comps[8*p+1].setName(newName);
			comps[8*p+3].setName(newName);
			comps[8*p+5].setName(newName);
			comps[8*p+6].setName(newName);
			comps[8*p+7].setName(newName);
			tf.setName(newName);
			l.setName(ovalue);			
			String cmd="delete_event("+condName+",["+paramName+"],["+kineName+"]).\n";
	    	cmd+="add_event("+condName+",["+ovalue+"],["+kineName+"]).\n";
	    	model.sendToBiocham(cmd,"events");	    	
	    	getEvents().remove(p);
	    	getEvents().add(p,new Event(condName,ovalue,kineName));
	    	l=null;
	    	tf=null;
	    	comps=null;
    		
    	}else{
    		
    		JLabel l=(JLabel)panel.getComponent(8*p+4);
    		String ovalue=getOrigKinetics().get(p);
			tf.setValue(ovalue);
			tf.setBackground(Color.WHITE);			
			String newName=condName+"-"+paramName+"-"+ovalue;
			Component[] comps=panel.getComponents();
			comps[8*p+1].setName(newName);
			comps[8*p+3].setName(newName);
			comps[8*p+5].setName(newName);
			comps[8*p+6].setName(newName);
			comps[8*p+7].setName(newName);
			tf.setName(newName);
			l.setName(ovalue);			
			String cmd="delete_event("+condName+",["+paramName+"],["+kineName+"]).\n";
	    	cmd+="add_event("+condName+",["+paramName+"],["+ovalue+"]).\n";
	    	model.sendToBiocham(cmd,"events");	    	
	    	getEvents().remove(p);
	    	getEvents().add(p,new Event(condName,paramName,ovalue));
    		
	    	l=null;
	    	tf=null;
	    	comps=null;
    	}
	}*/
	
	
	public int size() {
		return getEvents().size();
	}
	public void setModified(Component comp) {		
		comp.setBackground(Utils.modifiedParameterColor);	
	}
	
	
/*public class Event {
		
		
		private String condition, paramName, kinetics,paramValue;//ParamName can be a list of parameters....
		private String eventName;

		Event(String c, String n, String k) {
			
			condition=c;
	    	paramName=n;
	    	kinetics=k;	    	
			
	    }	     

	    
		public String getCondition() {
			return condition;
		}
		public void setCondition(String condition) {
			this.condition = new String(condition);
		}
		public String getKinetics() {
			return kinetics;
		}
		public void setKinetics(String kinetics) {
			this.kinetics = new String(kinetics);
		}
		public String getName() {
			return paramName;
		}
		public void setName(String name) {			
			this.paramName = new String(name);
		}
		
		public void resetValue() {					
		}		
		
		public String getParamValue() {
			return paramValue;
		}
		public void setParamValue(String paramValue) {
			this.paramValue = paramValue;
		}


		public String getEventName() {
			return condition+","+paramName+","+kinetics;
		}


		public void setEventName(String eventName) {
			this.eventName = eventName;
		}
}

*/
	
synchronized public JPanel refreshPanel(ArrayList cs) {
	
	
	SpringLayout layout = (SpringLayout) panel.getLayout();
	int rows = cs.size()/8;		
	
	for(int i=0;i<rows;i++){
		 
		JComponent cp=(JComponent) cs.get(8*i);		
		JLabel l1,l2,l3;
		JFormattedTextField tf1,tf2,tf3;
		
		l1=(JLabel)cp;				
		l2=(JLabel) cs.get(8*i+2);				
		l3=(JLabel) cs.get(8*i+4);			
		tf1 = (JFormattedTextField) cs.get(8*i+1);
		tf2 = (JFormattedTextField) cs.get(8*i+3);
		tf3 = (JFormattedTextField) cs.get(8*i+5);
		JLabel but1=(JLabel)cs.get(8*i+6);
		JLabel but2=(JLabel)cs.get(8*i+7);
					
		
		
		panel.add(l1);
		panel.add(tf1);	
		
		panel.add(l2);
		panel.add(tf2);
		
		panel.add(l3);
		panel.add(tf3)
		;
		panel.add(but1);			
		panel.add(but2);
		
		layout.putConstraint(SpringLayout.WEST, l1, 5, SpringLayout.WEST, panel);
        layout.putConstraint(SpringLayout.NORTH, l1, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);
          
        layout.putConstraint(SpringLayout.WEST, tf1, 10, SpringLayout.EAST, l1);
        layout.putConstraint(SpringLayout.NORTH, tf1, TOP_OFF+HEIGHT*i, SpringLayout.NORTH, panel);
          
        layout.putConstraint(SpringLayout.WEST, l2, 15, SpringLayout.EAST, tf1);
        layout.putConstraint(SpringLayout.NORTH, l2, TOP_OFF+HEIGHT*i, SpringLayout.NORTH, panel);
          
        layout.putConstraint(SpringLayout.WEST, tf2, 10, SpringLayout.EAST, l2);
        layout.putConstraint(SpringLayout.NORTH, tf2, TOP_OFF+HEIGHT*i, SpringLayout.NORTH, panel);
          
        layout.putConstraint(SpringLayout.WEST, l3, 15, SpringLayout.EAST, tf2);
        layout.putConstraint(SpringLayout.NORTH, l3, TOP_OFF+HEIGHT*i, SpringLayout.NORTH, panel);
          
        layout.putConstraint(SpringLayout.WEST, tf3, 10, SpringLayout.EAST, l3);
        layout.putConstraint(SpringLayout.NORTH, tf3, TOP_OFF+HEIGHT*i, SpringLayout.NORTH, panel);
        

        layout.putConstraint(SpringLayout.NORTH, but1, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);         
        layout.putConstraint(SpringLayout.WEST,  but1, 10,SpringLayout.EAST, tf3);
        
        layout.putConstraint(SpringLayout.NORTH, but2, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);
        layout.putConstraint(SpringLayout.WEST,  but2, 10,SpringLayout.EAST, but1);
         
        cp=null;
        l1=null;
        l2=null;
        l3=null;
        tf1=null;
        tf2=null;
        tf3=null;
        but1=null;
        but2=null;
        
	}
	
	
	String toolTipText="<html><i>Set up an event that will be fired each time the condition<br>" +
								"becomes true during the simulation. The given parameter at this<br>" +
								"time will be associated with the given value in the event declaration,<br>" +
								"but after the simulation ends it gets its initially definied value.<br>" +
								"Example: condition: Time>=10, <br> &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp name: k1, where k1 is a definied parameter," +
								"<br>&nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp kinetics: k4*10-ratio, where k4 and ratio are also definied" +
								"<br> &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp parameters.</i></html>";
    CustomToolTipButton b1=new CustomToolTipButton("Add",toolTipText); 
    b1.setBalloonToolTipVisible(false);
    b1.setName("Add");
    b1.setActionCommand("addEvent");
    b1.addActionListener(this);
  
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
         
		 /*layout.putConstraint(SpringLayout.NORTH, b1, BiochamModel.TOP_OFF+BiochamModel.HEIGHT*(size+3),SpringLayout.NORTH, panel);
		 Spring con=layout.getConstraint(SpringLayout.WEST, panel.getComponent((size-1)*8+6));
         layout.putConstraint(SpringLayout.WEST, b1, Spring.sum(con,Spring.constant(5)), SpringLayout.WEST, panel);
		 layout.putConstraint(SpringLayout.NORTH, b2, BiochamModel.TOP_OFF+BiochamModel.HEIGHT*(size+3),SpringLayout.NORTH, panel);
         layout.putConstraint(SpringLayout.WEST, b2, 10, SpringLayout.EAST, b1);*/
         
		 
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
			if((comps[i] instanceof JButton)){
				if(!((JButton)comps[i]).getActionCommand().equals("addEvent")){
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
	
	public void disposeElements() {
		
		getEvents().clear();
		setEvents(null);
		biocham=null;
		panel=null;
		model=null;
		element=null;
		savedResponses=0;
		if(origCondition!=null){
			origCondition.clear();
		}
		origCondition=null;
		if(origName!=null){
			origName.clear();
		}
		origName=null;
		if(origKinetics!=null){
			origKinetics.clear();
		}
		origKinetics=null;
		
	}
	
	
	
	public boolean isModifying() {
		return modifying;
	}
	public void setModifying(boolean modifying) {
		this.modifying = modifying;
	}
	public void resetParameter(String s) {
				
	}
	public static ArrayList<String> getOrigCondition() {
		return origCondition;
	}
	public static ArrayList<String> getOrigKinetics() {
		return origKinetics;
	}	public static ArrayList<String> getOrigName() {
		return origName;
	}

	public void actionPerformed(ActionEvent e) {
		
		if(e.getActionCommand()=="addEvent"){
			
			addParameter();
		}else if(e.getActionCommand().equals("deleteAllEvents")){
		//	deleteAllEvents();
		}
	}

	/*	public void deleteAllEvents(){
	for(int i=0;i<getEvents().size();i++){
			model.sendToBiocham("delete_event("+getEvents().get(i).getEventName()+").\n");
		}
		getEvents().clear();
		panel.removeAll();	
	
		ArrayList cs=new ArrayList(0);
		panel=refreshPanel(cs);			
		cs.clear();
		cs=null;			
		panel.setPreferredSize(new Dimension(getPanelCurrentX(),getPanelCurrentY()));	
		panel.revalidate();
	}*/

	public void addParameter() {
		
		setNewAdded(true);
		setModifying(false);
		setSavedResponses(-1);
		DialogAddParameter param=new DialogAddParameter(BiochamMainFrame.frame,model.getEvents(),"event",null,panel);	
		Object[] newParam=new Object[2];
		newParam=param.getNewParameter();
		param=null;				
		String name=(String)newParam[0]; // name = "condition-paramNames"
		String value=(String)newParam[1]; // value = kinetics
		newParam=null;
		
		if(name!=null && value!=null){
			
			int ind=name.indexOf('-');
			String condition=name.substring(0,ind),paramName=name.substring(ind+1);	
			String newName=condition+"-"+paramName+"-"+value;
			
		    if (checkExistence(newName,panel)<0) {
		    
		    	String s="add_event("+condition+",["+paramName+"],["+value+"]).\n";
				model.sendToBiocham(s,"events");
		    }else{
		    	JOptionPane.showMessageDialog(BiochamMainFrame.frame, "The event already exists.");
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
		
		
	}



	public boolean isModiff() {
		return modiff;
	}



	public void setModiff(boolean modiff) {
		this.modiff = modiff;
	}
	public void setEvents(Vector<Event> events) {
		this.events = events;
	}
	public Vector<Event> getEvents() {
		return events;
	}
	public int indexOf(String paramName) {
		// TODO Auto-generated method stub
		return 0;
	}
	
}
