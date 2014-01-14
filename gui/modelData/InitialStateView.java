package fr.inria.contraintes.biocham.modelData;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.TreeSet;
import fr.inria.contraintes.biocham.WorkbenchToolBars;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.customComponents.ModifyButton;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.Utils;

import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JFormattedTextField;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.Spring;
import javax.swing.SpringLayout;

import net.java.balloontip.BalloonTip;



/**
 * The view displays the data and executes on view clicking events.
 * 
 * */
public class InitialStateView  extends ParametersPanel{

	
	InitialStateModel initStateModel;
	static String elementName="Initial State";	
	CustomToolTipButton add,allAbs, allPres, absNotPres, presNotAbs, allUndef;	
	JPanel buttonsPanel,centralPanel;	
	JFrame parentFrame;
	
	
	public JPanel getCentralPanel() {
		return centralPanel;
	}

	public void setCentralPanel(JPanel centralPanel) {
		this.centralPanel = centralPanel;
	}

	InitialStateController listener;
	MouseListener mouseListener;
	KeyListener keyListener;
	BalloonTip bTip=null;	
	
	/**
	 * Constructor that memorizes the reference to the initial state data,
	 * and calls a method the constructs the panel's graphical view. 
	 * */
	public InitialStateView(JFrame parent,InitialStateModel model){
		super(elementName);		
		parentFrame=parent;
		initStateModel=model;	
		listener=new InitialStateController(initStateModel,this);
		mouseListener=listener.getMouseListener();	
		keyListener=listener.getKeyListener();
		initStateModel.getViews().add(this);
		constructGUI();
		
	}
	
	/**
	 * Constructs the panel. It contains 2 subpanels: 
	 * One at the bottom that contains the event buttons (buttonsPanel), and 
	 * the second at the center (centarPanel) that lists the initial state in the model.
	 * */
	private void constructGUI(){
		
			
		centralPanel=new JPanel(new SpringLayout());
		buttonsPanel=new JPanel(new FlowLayout(FlowLayout.LEADING));		
		centralPanel.setBackground(Utils.backgroundColor);
		buttonsPanel.setBackground(Utils.backgroundColor);
		constructButtonsPanel();
		constructInitStatePanel();
		add(centralPanel,BorderLayout.CENTER);
		add(buttonsPanel,BorderLayout.SOUTH);
		
	}
	
	/**
	 * Creates the buttons for adding a new initial state,setting values to initial state, etc.
	 * 
	 * */
	private void constructButtonsPanel() {
		
		
		String toolTipText="<html><i>Define the initial state of an object.<br>Example: (MA,0.035) or (MA,k1) where k1 is a definied parameter.</i></html>";
		add=new CustomToolTipButton("Add",toolTipText);	
		add.setBalloonToolTipVisible(false);
		add.setName("Add");
		
		add.setActionCommand("addInitConc");
		
		toolTipText="<html><i>Makes all objects declared not present, absent in the initial state.</i></html>";
		absNotPres=new CustomToolTipButton("Make Absent not Present",toolTipText);	
		absNotPres.setBalloonToolTipVisible(false);
		absNotPres.setName("absentNotPresent");
		absNotPres.setActionCommand("absentNotPresent");
	    
	    toolTipText="<html><i>Makes all objects declared not absent, present in the initial state.</i></html>";
	    presNotAbs=new CustomToolTipButton("Make Present not Absent",toolTipText);	
	    presNotAbs.setBalloonToolTipVisible(false);
	    presNotAbs.setName("presentNotAbsent");
	    presNotAbs.setActionCommand("presentNotAbsent");
	    
	    toolTipText="<html><i>Makes all objects absent from the initial state.</i></html>";
	    allAbs=new CustomToolTipButton("All Absent",toolTipText);	
	    allAbs.setBalloonToolTipVisible(false);
	    allAbs.setName("allAbsent");
	    allAbs.setActionCommand("allAbsent");	 
	    
	    toolTipText="<html><i>Makes all objects present in the initial state.</i></html>";
	    allPres=new CustomToolTipButton("All Present",toolTipText);	
	    allPres.setBalloonToolTipVisible(false);
	    allPres.setName("allPresent");
	    allPres.setActionCommand("allPresent");	  
	    
	    toolTipText="<html><i>Makes all objects possibly present or absent in the initial state.</i></html>";
	    allUndef=new CustomToolTipButton("All Undefined",toolTipText);	
	    allUndef.setBalloonToolTipVisible(false);
	    allUndef.setName("allUndef");
	    allUndef.setActionCommand("allUndef");	   	    
	  	    
	    allUndef.addActionListener(listener);
	    allPres.addActionListener(listener);
	    allAbs.addActionListener(listener);
	    presNotAbs.addActionListener(listener);
	    absNotPres.addActionListener(listener);
	    add.addActionListener(listener);
	    
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
	    buttonsPanel.add(absNotPres);
	    buttonsPanel.add(presNotAbs);
	    buttonsPanel.add(allAbs);
	    buttonsPanel.add(allPres);
	    buttonsPanel.add(allUndef);
	    buttonsPanel.add(refreshButton);
		
	}
	
	/**
	 * Refreshes the central panel. Removes all initial state content, and re-creates it with the current initialStateModel data.
	 * 
	 * */
	@Override
	public void refresh(){
		centralPanel.removeAll();
		constructInitStatePanel();
		repaint();
	
	}
	
	/**
	 * Constructs the central panel that lists all the initial state currently defined in the model.
	 * 
	 * */
	private void constructInitStatePanel() {
		
		int size=initStateModel.getInitStates().size();		
		if(size>0){
			
			SpringLayout layout=(SpringLayout) centralPanel.getLayout();			
			
		
			int northBefore=20;			
		
						
			for(String parent: initStateModel.getInitStates().keySet()){//new TreeSet<String>(){				
				
				JFormattedTextField tf=new JFormattedTextField();
				tf.setColumns(10);
				tf.setName(parent);
				if(initStateModel.getStartInitialState().containsKey(parent)){					
					if(initStateModel.getStartInitialState().get(parent)!=initStateModel.getInitStates().get(parent)){						
						tf.setBackground(Utils.modifiedParameterColor);						
					}
				}
				tf.setValue(initStateModel.getInitStates().get(parent));
				tf.setHorizontalAlignment(JTextField.LEFT);
				tf.setEditable(false);
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
				tf.addMouseListener(mouseListener);
				tf.addKeyListener(keyListener);
				JLabel l = new JLabel(parent);
				l.setName(parent);	
				l.setLabelFor(tf);
				ModifyButton but1=new ModifyButton();				
		    	but1.setName(parent);
		    	but1.addMouseListener(mouseListener);
		    	
		    	String butName="Present";
		    	double val=0.0;
		    	String ss=null;
		    	try{
		    		ss=initStateModel.getInitStates().get(parent);
		    		val=Double.parseDouble(ss);
		    		
		    	}catch(Exception e){
		    		/* System.out.println("The value not of numerical type...."+val+"?"+ss);
					 WorkbenchToolBars.infoLabel.setText("The value is not of numerical type...."+val+"?");
					 WorkbenchToolBars.infoLabel.setBalloonToolTipVisible(true);*/
		    	}
		    	ss=null;
		    	if(val>0){
		    		butName="Absent";
		    	}
		    	JButton but2=new JButton(butName);
		    	but2.setActionCommand(butName);
		    	but2.setName(parent);	    	
		    	but2.addActionListener(listener);	    
		    	
		    	/*JButton but3=new JButton("Undefined");
		    	but3.setActionCommand("Undefined");
		    	but3.setName(parent);	
		    	but3.addActionListener(listener);*/
		    	
		    	
		    	centralPanel.add(l);
				centralPanel.add(tf);
		    	centralPanel.add(but1);
		    	centralPanel.add(but2);	 
		    	//centralPanel.add(but3);	 
		    	
		    	layout.putConstraint(SpringLayout.WEST, l, 5, SpringLayout.WEST, centralPanel);
		    	layout.putConstraint(SpringLayout.WEST, tf, 5, SpringLayout.EAST, l);
				layout.putConstraint(SpringLayout.NORTH, l, northBefore,SpringLayout.NORTH, centralPanel);				
				layout.putConstraint(SpringLayout.NORTH, tf, northBefore,SpringLayout.NORTH, centralPanel);
				
				layout.putConstraint(SpringLayout.WEST, but1, 10, SpringLayout.EAST, tf);
				layout.putConstraint(SpringLayout.NORTH, but1, northBefore,SpringLayout.NORTH, centralPanel);
				layout.putConstraint(SpringLayout.WEST, but2, 5, SpringLayout.EAST, but1);
				layout.putConstraint(SpringLayout.NORTH, but2, northBefore,SpringLayout.NORTH, centralPanel);
				/*layout.putConstraint(SpringLayout.WEST, but3, 5, SpringLayout.EAST, but2);
				layout.putConstraint(SpringLayout.NORTH, but3, northBefore,SpringLayout.NORTH, centralPanel); */
				  
				Spring maxSpring = Spring.constant(10);	
				int ccnt=centralPanel.getComponentCount();
				int s=ccnt/4;
		    	for (int i=0;i<s;i++)
		    		maxSpring = Spring.max(maxSpring, Spring.sum(layout.getConstraints(centralPanel.getComponent(4*i)).getWidth(),Spring.constant(10)));
		    	for (int i=0;i<s;i++)
		    		layout.putConstraint(SpringLayout.WEST, centralPanel.getComponent(4*i+1), maxSpring, SpringLayout.WEST, centralPanel);
		       			
		    	maxSpring=null;
		    	northBefore+=30;
			}	
			centralPanel.setPreferredSize(new Dimension(50,northBefore+60));		
			this.revalidate();
		}
	}
	public JFrame getParentFrame() {
		return parentFrame;
	}
	public void setParentFrame(JFrame parentFrame) {
		this.parentFrame = parentFrame;
	}
}