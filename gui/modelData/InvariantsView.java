package fr.inria.contraintes.biocham.modelData;

import net.java.balloontip.BalloonTip;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.customComponents.DeleteButton;
import fr.inria.contraintes.biocham.modelData.InvariantsController.DeleteInvariant;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.Utils;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
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
public class InvariantsView extends ParametersPanel{

	
	InvariantsModel invariantsModel;
	static String elementName="Invariants";	
	CustomToolTipButton add;	
	JPanel buttonsPanel,centralPanel;	
	InvariantsController listener;
	DeleteInvariant deleteListener;
	BalloonTip bTip=null;	
	JFrame parentFrame;
	/**
	 * Constructor that memorizes the reference to the Invariants data,
	 * and calls a method the constructs the panel's graphical view. 
	 * */
	public InvariantsView(JFrame parent,InvariantsModel model){
		super(elementName);		
		parentFrame=parent;
		invariantsModel=model;	
		listener=new InvariantsController(invariantsModel,this);			
		deleteListener=listener.getDeleteListener();		
		invariantsModel.getViews().add(this);
		constructGUI();		
	}
	
	/**
	 * Constructs the panel. It contains 2 subpanels: 
	 * One at the bottom that contains the event buttons (buttonsPanel), and 
	 * the second at the center (centarPanel) that lists the Invariants in the model.
	 * */
	private void constructGUI(){
		
			
		centralPanel=new JPanel(new SpringLayout());
		buttonsPanel=new JPanel(new FlowLayout(FlowLayout.LEADING));		
		centralPanel.setBackground(Utils.backgroundColor);
		buttonsPanel.setBackground(Utils.backgroundColor);
		constructButtonsPanel();
		constructInvariantsPanel();
		add(centralPanel,BorderLayout.CENTER);
		add(buttonsPanel,BorderLayout.SOUTH);
		
	}
	
	/**
	 * Creates the buttons for adding a new Invariant,deleting, searching, cecking, etc.
	 * 
	 * */
	private void constructButtonsPanel() {		
		
		String toolTipText="<html><i>Declares a new mass conservation law for all the molecules matched <br>" +
 		"by the argument if it is a set, or for all molecules given with the <br> corresponding weight." +
 		"<br>Example: {cycE-?} or [A-A, 2*A]. </i></html>";
		add=new CustomToolTipButton("Add",toolTipText);	
		add.setBalloonToolTipVisible(false);
		add.setName("Add");
		add.setActionCommand("addInvariant");		
	    add.addActionListener(listener);
	    
	    toolTipText="<html><i>Will check all conservation laws against rules, and if necessary kinetics.</i><html>";
	    CustomToolTipButton b2=new CustomToolTipButton("Check",toolTipText);
	    b2.setName("CheckConservationLaw");		
	    b2.setActionCommand("checkConservationLaw");
	    b2.addActionListener(listener);
		b2.setBalloonToolTipVisible(false);
		
		toolTipText="<html><i>Computes the P-invariants of the system, and thus conservation laws <br>" +
				"that are independent from the precise kinetics. One can give a limit on the highest value " +
				"<br> found in a given P-invariant (the default is 4), but note that giving a too high value <br>" +
				"might lead to a very long computation.</i></html>";
		CustomToolTipButton b3=new CustomToolTipButton("Search",toolTipText);
	    b3.setName("P-invariants");
	    b3.setActionCommand("P-invariants");
	    b3.addActionListener(listener);	
	    b3.setBalloonToolTipVisible(false);
	    
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
	    buttonsPanel.add(b2);
	    buttonsPanel.add(b3);
	    buttonsPanel.add(refreshButton);
	    
	}
	
	/**
	 * Refreshes the central panel. Removes all Invariants content, and re-creates it with the current InvariantsModel data.
	 * 
	 * */
	public void refresh(){
		centralPanel.removeAll();
		constructInvariantsPanel();
		repaint();
	
	}
	/**
	 * Constructs the central panel that lists all the Invariants currently defined in the model.
	 * 
	 * */
	private void constructInvariantsPanel() {
		
		int size=invariantsModel.getInvariants().size();		
		if(size>0){
			
			SpringLayout layout=(SpringLayout) centralPanel.getLayout();
			int northBefore=20;
			String parent=null;
			for(int i=0; i<invariantsModel.getInvariants().size();i++){				
				parent=invariantsModel.getInvariants().get(i);
				JFormattedTextField tf=new JFormattedTextField();
				tf.setColumns(30);
				tf.setName(parent);
				tf.setValue(parent);
				tf.setHorizontalAlignment(JTextField.LEFT);
				tf.setEditable(false);				
		    	DeleteButton but2=new DeleteButton();
		    	but2.setName(parent);		    
		    	but2.addMouseListener(deleteListener);    
		    			    			    	
				centralPanel.add(tf);
		    	centralPanel.add(but2);	 
		    	
		    	
		    	layout.putConstraint(SpringLayout.WEST, tf, 5, SpringLayout.WEST, centralPanel);
				layout.putConstraint(SpringLayout.NORTH, tf, northBefore,SpringLayout.NORTH, centralPanel);				
				layout.putConstraint(SpringLayout.WEST, but2, 10, SpringLayout.EAST, tf);
				layout.putConstraint(SpringLayout.NORTH, but2, northBefore,SpringLayout.NORTH, centralPanel);
							
		    	northBefore+=30;
			}	
			centralPanel.setPreferredSize(new Dimension(50,northBefore+60));		
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

