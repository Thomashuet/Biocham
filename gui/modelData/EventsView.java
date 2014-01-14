package fr.inria.contraintes.biocham.modelData;

import net.java.balloontip.BalloonTip;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.customComponents.DeleteButton;
import fr.inria.contraintes.biocham.modelData.EventsController.DeleteEvent;
import fr.inria.contraintes.biocham.modelData.EventsModel.Event;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.Utils;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.JFormattedTextField;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SpringLayout;



/**
 * The view displays the data and executes on view clicking events.
 * 
 * */
public class EventsView extends ParametersPanel{

	
	EventsModel eventsModel;
	static String elementName="Events";	
	CustomToolTipButton add,addTE;	
	JPanel buttonsPanel,centralPanel;	
	EventsController listener;
	DeleteEvent deleteListener;	
	BalloonTip bTip=null;	
	JFrame parentFrame;
	
	/**
	 * Constructor that memorizes the reference to the Events data,
	 * and calls a method the constructs the panel's graphical view. 
	 * */
	public EventsView(JFrame parent,EventsModel model){
		super(elementName);
		parentFrame=parent;
		eventsModel=model;	
		listener=new EventsController(eventsModel,this);		
		deleteListener=listener.getDeleteListener();		
		eventsModel.getViews().add(this);
		constructGUI();
		
	}
	
	/**
	 * Constructs the panel. It contains 2 subpanels: 
	 * One at the bottom that contains the event buttons (buttonsPanel), and 
	 * the second at the center (centarPanel) that lists the Events in the model.
	 * */
	private void constructGUI(){
		
			
		centralPanel=new JPanel(new SpringLayout());
		buttonsPanel=new JPanel(new FlowLayout(FlowLayout.LEADING));		
		centralPanel.setBackground(Utils.backgroundColor);
		buttonsPanel.setBackground(Utils.backgroundColor);
		constructButtonsPanel();
		constructEventsPanel();
		add(centralPanel,BorderLayout.CENTER);
		add(buttonsPanel,BorderLayout.SOUTH);
		
	}
	
	/**
	 * Creates the buttons for adding a new Event,deleting, etc.
	 * 
	 * */
	private void constructButtonsPanel() {		
		
		String toolTipText="<html><i>Define an Event.<br> Example: Time>=10; k1,k2; 50,25; </i></html>";
		add=new CustomToolTipButton("Add Event",toolTipText);	
		add.setBalloonToolTipVisible(false);
		add.setName("AddEvent");
		add.setActionCommand("addEvent");		
	    add.addActionListener(listener);
	    
	    toolTipText="<html><i>Define a time event.<br> Example: 2.0; true; k1,k2; 50,25; </i></html>";
		addTE=new CustomToolTipButton("Add Time Event",toolTipText);	
		addTE.setBalloonToolTipVisible(false);
		addTE.setName("AddTimeEvent");
		addTE.setActionCommand("addTimeEvent");		
		addTE.addActionListener(listener);
	    
	    JLabel refreshButton=new JLabel();
	    refreshButton.setIcon(Icons.icons.get("Refresh3.png"));		
	    refreshButton.setName("refresh");	    
	    refreshButton.setText("Screen Refresh");
	    refreshButton.setForeground(Utils.refreshedColor);
	    refreshButton.setToolTipText("Click to Refresh the Screen");
	    refreshButton.addMouseListener(new MouseAdapter(){
	    	public void mouseClicked(MouseEvent me) { 
	            refresh();
	        }
	    });	    
	    buttonsPanel.add(add);
	    buttonsPanel.add(addTE);
	    buttonsPanel.add(refreshButton);		
	}
	
	/**
	 * Refreshes the central panel. Removes all Events content, and re-creates it with the current EventsModel data.
	 * 
	 * */
	public void refresh(){
		centralPanel.removeAll();
		constructEventsPanel();
		repaint();
	
	}
	
	
	/**
	 * Constructs the central panel that lists all the Events currently defined in the model.
	 * 
	 * */
	private void constructEventsPanel() {
		
		int size=eventsModel.getEvents().size();		
		if(size>0){
			
			SpringLayout layout=(SpringLayout) centralPanel.getLayout();
			int northBefore=20;			
			Event e=null;
			for(int i=0;i<eventsModel.getEvents().size();i++){				
				
				e=eventsModel.getEvents().get(i);
				JLabel time=null;
				JFormattedTextField timeText=null;
				if(e.getFixedTime()!=null){
					time=new JLabel("Time:");
					timeText=new JFormattedTextField();
					timeText.setColumns(10);
					timeText.setName(e.toString());
					timeText.setValue(e.getFixedTime());
					timeText.setHorizontalAlignment(JTextField.LEFT);
					timeText.setEditable(false);
				}
				JLabel lc = new JLabel("Condition: ");
				lc.setName(e.toString());				
				JFormattedTextField tfc=new JFormattedTextField();
				tfc.setColumns(10);
				tfc.setName(e.toString());
				tfc.setValue(e.getCondition());
				tfc.setHorizontalAlignment(JTextField.LEFT);
				tfc.setEditable(false);
				
				JLabel lp = new JLabel("Names:    ");
				lp.setName(e.toString());				
				JFormattedTextField tfp=new JFormattedTextField();
				tfp.setColumns(10);
				tfp.setName(e.toString());
				tfp.setValue(e.getParameters());
				tfp.setHorizontalAlignment(JTextField.LEFT);
				tfp.setEditable(false);
				
				JLabel lk = new JLabel("Kinetics:  ");
				lk.setName(e.toString());				
				JFormattedTextField tfk=new JFormattedTextField();
				tfk.setColumns(10);
				tfk.setName(e.toString());
				tfk.setValue(e.getKinetics());
				tfk.setHorizontalAlignment(JTextField.LEFT);
				tfk.setEditable(false);
				
		    	DeleteButton but2=new DeleteButton();
		    	but2.setName(e.toString());		    
		    	but2.addMouseListener(deleteListener);    
		    	
		    	
		    	centralPanel.add(but2);			    	
		    	if(e.getFixedTime()!=null){
		    		centralPanel.add(time);
		    		centralPanel.add(timeText);
		    	}
		    	centralPanel.add(lc);
				centralPanel.add(tfc);
				centralPanel.add(lp);
				centralPanel.add(tfp);
				centralPanel.add(lk);
				centralPanel.add(tfk);		    	
	 
				
				layout.putConstraint(SpringLayout.NORTH, but2, northBefore,SpringLayout.NORTH, centralPanel);
				layout.putConstraint(SpringLayout.NORTH, lc, northBefore+3,SpringLayout.NORTH, centralPanel);		
				layout.putConstraint(SpringLayout.NORTH, tfc, northBefore,SpringLayout.NORTH, centralPanel);		    	
		    	layout.putConstraint(SpringLayout.NORTH, lp, northBefore+3,SpringLayout.NORTH, centralPanel);
		    	layout.putConstraint(SpringLayout.NORTH, tfp, northBefore,SpringLayout.NORTH, centralPanel);
		    	layout.putConstraint(SpringLayout.NORTH, lk, northBefore+3,SpringLayout.NORTH, centralPanel);
		    	layout.putConstraint(SpringLayout.NORTH, tfk, northBefore,SpringLayout.NORTH, centralPanel);	
		    	
		    	
		    	layout.putConstraint(SpringLayout.WEST,  but2, 5,SpringLayout.WEST, centralPanel);
		    	
		    	if(e.getFixedTime()!=null){
		    		layout.putConstraint(SpringLayout.NORTH, time, northBefore+3,SpringLayout.NORTH, centralPanel);		
					layout.putConstraint(SpringLayout.NORTH, timeText, northBefore,SpringLayout.NORTH, centralPanel);	
					layout.putConstraint(SpringLayout.WEST,  time, 15, SpringLayout.EAST, but2);
		    		layout.putConstraint(SpringLayout.WEST,  timeText, 4, SpringLayout.EAST, time);
		    		layout.putConstraint(SpringLayout.WEST,  lc, 7, SpringLayout.EAST, timeText);		    	    	
			    	layout.putConstraint(SpringLayout.WEST,  tfc, 4, SpringLayout.EAST, lc);		    	
			    	layout.putConstraint(SpringLayout.WEST,  lp, 7, SpringLayout.EAST, tfc);		    	
			    	layout.putConstraint(SpringLayout.WEST,  tfp, 3, SpringLayout.EAST, lp);		    	
			    	layout.putConstraint(SpringLayout.WEST,  lk, 7, SpringLayout.EAST, tfp);		    
			    	layout.putConstraint(SpringLayout.WEST,  tfk, 4, SpringLayout.EAST, lk);	  
		    	}else{
		    		layout.putConstraint(SpringLayout.WEST,  lc, 15, SpringLayout.EAST, but2);		    	    	
			    	layout.putConstraint(SpringLayout.WEST,  tfc, 4,SpringLayout.EAST, lc);		    	
			    	layout.putConstraint(SpringLayout.WEST,  lp, 10,SpringLayout.EAST, tfc);		    	
			    	layout.putConstraint(SpringLayout.WEST,  tfp, 4,SpringLayout.EAST, lp);		    	
			    	layout.putConstraint(SpringLayout.WEST,  lk, 10,SpringLayout.EAST, tfp);		    
			    	layout.putConstraint(SpringLayout.WEST,  tfk, 4,SpringLayout.EAST, lk);	  
		    	}
		    	
		    	
		    	  	    	
		    	
				 
				
		    	northBefore+=30;
			}	
			centralPanel.setPreferredSize(new Dimension(800,northBefore+60));		
			this.revalidate();
		}
	}

	public JPanel getCentralPanel() {
		return centralPanel;
	}

	public void setCentralPanel(JPanel centralPanel) {
		this.centralPanel = centralPanel;
	}
	public JFrame getParentFrame() {
		return parentFrame;
	}
	public void setParentFrame(JFrame parentFrame) {
		this.parentFrame = parentFrame;
	}
	
}

