package fr.inria.contraintes.biocham.modelData;

import net.java.balloontip.BalloonTip;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.Utils;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SpringLayout;



/**
 * The view displays the data and executes on view clicking events.
 * 
 * */
public class MoleculesView extends ParametersPanel{

	
	MoleculesModel moleculesModel;
	static String elementName="Molecules";	
	CustomToolTipButton check;	
	JPanel buttonsPanel,centralPanel;	
	MoleculesController listener;	
	JFrame parentFrame;
	BalloonTip bTip=null;	
	
	/**
	 * Constructor that memorizes the reference to the Molecules data,
	 * and calls a method the constructs the panel's graphical view. 
	 * */
	public MoleculesView(JFrame parent,MoleculesModel model){
		super(elementName);		
		parentFrame=parent;
		moleculesModel=model;	
		listener=new MoleculesController(moleculesModel,this);	
		moleculesModel.getViews().add(this);
		constructGUI();
	
	}
	
	/**
	 * Constructs the panel. It contains 2 subpanels: 
	 * One at the bottom that contains the event buttons (buttonsPanel), and 
	 * the second at the center (centarPanel) that lists the Molecules in the model.
	 * */
	private void constructGUI(){
		
			
		centralPanel=new JPanel(new SpringLayout());
		buttonsPanel=new JPanel(new FlowLayout(FlowLayout.LEADING));		
		centralPanel.setBackground(Utils.backgroundColor);
		buttonsPanel.setBackground(Utils.backgroundColor);
		constructButtonsPanel();
		constructMoleculesPanel();
		add(centralPanel,BorderLayout.CENTER);
		add(buttonsPanel,BorderLayout.SOUTH);
		
	}
	
	/**
	 * Creates the buttons for adding a new Invariant,deleting, searching, cecking, etc.
	 * 
	 * */
	private void constructButtonsPanel() {		
		
		String toolTipText="<html><i>Checks the lower/upper case errors in molecules' names,<br> and checks if there are production and " +
		"degradation <br> rules for them.</i></html>";
		check=new CustomToolTipButton("Check",toolTipText);	
		check.setBalloonToolTipVisible(false);
		check.setName("Check");
		check.setActionCommand("checkMolecules");		
		check.addActionListener(listener);
		
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
	    buttonsPanel.add(check);
	    buttonsPanel.add(refreshButton);
	    
	}
	
	/**
	 * Refreshes the central panel. Removes all Molecules content, and re-creates it with the current MoleculesModel data.
	 * 
	 * */
	public void refresh(){
		centralPanel.removeAll();
		constructMoleculesPanel();
		repaint();
	
	}
	/**
	 * Constructs the central panel that lists all the Molecules currently defined in the model.
	 * 
	 * */
	private void constructMoleculesPanel() {
		
		int size=moleculesModel.getMolecules().size();		
		if(size>0){
			
			SpringLayout layout=(SpringLayout) centralPanel.getLayout();
			int northBefore=20;
			String parent=null;
			for(int i=0; i<moleculesModel.getMolecules().size();i++){				
				parent=moleculesModel.getMolecules().get(i);
				JTextField tf = new JTextField(parent);
				tf.setEditable(false);
				tf.setBackground(Utils.backgroundColor);
				tf.setName(parent);  
				
				centralPanel.add(tf);    	
		    	
		    	
		    	layout.putConstraint(SpringLayout.WEST, tf, 5, SpringLayout.WEST, centralPanel);
				layout.putConstraint(SpringLayout.NORTH, tf, northBefore,SpringLayout.NORTH, centralPanel);				
											
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

