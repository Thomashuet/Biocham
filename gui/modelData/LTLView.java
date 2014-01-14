/**
 * 
 */
package fr.inria.contraintes.biocham.modelData;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFormattedTextField;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SpringLayout;

import net.java.balloontip.BalloonTip;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.customComponents.DeleteButton;
import fr.inria.contraintes.biocham.customComponents.UniversalScrollPane;
import fr.inria.contraintes.biocham.modelData.LTLController.DeleteProperty;
import fr.inria.contraintes.biocham.utils.Utils;

/**
 * The view displays the LTL data and executes corresponding functions on view clicking events.
 * 
 * There is only one FIX view per model that is attached to the ModelEditorPanel, and there can be any number of popup generated views.
 * The FIX view and the generated popup view share the same model. 
 * There is only one popup view per time. When the frame is being closed, its disposed, and when generated again, new view is created as a model representation.  
 * The model contains the list of specifications and the generated results from their manipulation.
 *  
 * @author Dragana Jovanovska
 *  
 * */
public class LTLView extends PopupSplitPane{

	static String elementName="Numerical Temporal properties";	
	
	LTLModel ltlModel;
	
	CustomToolTipButton add;	
	LTLController listener;
	DeleteProperty deleteListener;
	BalloonTip bTip=null;
	SpringLayout topLeftPanelLayout;
	JTextArea tarea;		
	
	TopLeftLTLPanelFirst topLeftPanel;
	JPanel topRightPanel,resultsPanel;	
	JPanel ltlSpecPanel,ltlBottonsPanel;	
	JPanel resultsButtonsPanel;	
	JPanel topLeftAll;	
	JFrame parentFrame;
	/**
	 * Constructor that memorizes the reference to the LTL data,
	 * and calls a method the constructs the panel's graphical view. 
	 * */
	public LTLView(JFrame parent, LTLModel model){
		super(JSplitPane.VERTICAL_SPLIT);		
		parentFrame=parent;
		ltlModel=model;	
		listener=new LTLController(ltlModel,this);			
		deleteListener=listener.getDeleteListener();		
		ltlModel.getViews().add(this);
		constructGUI();
		Utils.debugMsg("\n\n ***** Number of views="+ltlModel.getViews().size());
		
	}
	
	
	/**
	 * Constructs the panel. 
	 * It contains 2 major panels, divided horizontally the top and the bottom. From there, the top is divided vertically to the left and the right panel. 
	 * The top left panel contains the actions commands' buttons.
	 * The top right contains the specifications introduced to the model.
	 * The bottom panel represents an output area for the executed model checking commands.
	 * 
	 * */
	private void constructGUI(){
		
		constructTopLeftPanel(); //OK
		constructTopRightPanel();	 //OK	
		constructBottomPanel();		
		JSplitPane upLTL=new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,new UniversalScrollPane(topLeftAll),new UniversalScrollPane(topRightPanel));		
		upLTL.setResizeWeight(0.2);
		upLTL.setDividerLocation(0.5);
		upLTL.setDividerSize(1);
		upLTL.setContinuousLayout(true);
		setOrientation(JSplitPane.VERTICAL_SPLIT);
		setLeftComponent(upLTL);	
		setRightComponent(new UniversalScrollPane(resultsPanel));
		setResizeWeight(0.5);			
		setDividerLocation(0.5);
		setDividerSize(10);
			
	}
	
	/**
	 * Creates top left panel with the model checking commands bottons
	 * 
	 * */
	private void constructTopLeftPanel() {		
		topLeftAll=new JPanel(new BorderLayout());	
		JPanel center=new JPanel(new GridLayout(2,1));		
		topLeftPanel=new TopLeftLTLPanelFirst(listener);
		center.add(new TopLeftLTLPanelZero(listener));
		center.add(topLeftPanel);		
		topLeftAll.add(center,BorderLayout.NORTH);
		topLeftAll.add(new TopLeftLTLPanelSecond(listener),BorderLayout.CENTER);
				
	}
	
	/**
	 * Creates top right panel with the model LTL Specifications
	 * 
	 * */
	private void constructTopRightPanel() {		
		if(topRightPanel==null){
			topRightPanel=new JPanel(new BorderLayout());
			topRightPanel.setBackground(Utils.backgroundColor);
		}else{
			topRightPanel.removeAll();
		}	
		constructLTLSpecPanel();
		constructButtonsPanel();
		
		topRightPanel.add(ltlSpecPanel,BorderLayout.CENTER);		
		topRightPanel.add(ltlBottonsPanel,BorderLayout.NORTH);
	}
	
	/**
	 * Constructs the LTL Specifications panel that lists all the LTL currently defined in the model.
	 * 
	 * */
	private void constructLTLSpecPanel() {
		
		if(ltlSpecPanel==null){
			ltlSpecPanel=new JPanel(new SpringLayout());
			ltlSpecPanel.setBackground(Utils.backgroundColor);
		}else{
			ltlSpecPanel.removeAll();
		}		
		int size=ltlModel.getSpecifications().size();
		
		if(size>0){
			
			SpringLayout layout=(SpringLayout)ltlSpecPanel.getLayout();
			int northBefore=20;
			String parent=null;
			for(int i=0; i<ltlModel.getSpecifications().size();i++){				
				parent=ltlModel.getSpecifications().get(i);
				JFormattedTextField tf=new JFormattedTextField();
				tf.setColumns(30);
				tf.setName(parent);
				tf.setValue(parent);
				tf.setHorizontalAlignment(JTextField.LEFT);
				tf.setEditable(false);				
		    	DeleteButton but2=new DeleteButton();
		    	but2.setName(parent);		    
		    	but2.addMouseListener(deleteListener);    
		    			    			    	
		    	ltlSpecPanel.add(tf);
		    	ltlSpecPanel.add(but2);	 		    	
		    	
		    	layout.putConstraint(SpringLayout.WEST, tf, 5, SpringLayout.WEST, ltlSpecPanel);
				layout.putConstraint(SpringLayout.NORTH, tf, northBefore,SpringLayout.NORTH, ltlSpecPanel);				
				layout.putConstraint(SpringLayout.WEST, but2, 10, SpringLayout.EAST, tf);
				layout.putConstraint(SpringLayout.NORTH, but2, northBefore,SpringLayout.NORTH, ltlSpecPanel);
							
		    	northBefore+=30;
			}	
			ltlSpecPanel.setPreferredSize(new Dimension(50,northBefore+60));		
			this.revalidate();
		}
	}
	
	/**
	 * Creates the buttons for adding a new LTL property,generating automatically some or adding automatically generated ones, revise/learn/reduce rules.
	 * 
	 * */
	private void constructButtonsPanel() {
		
		
		if(ltlBottonsPanel==null){
			FlowLayout fl=new FlowLayout(FlowLayout.LEFT);			
			ltlBottonsPanel=new JPanel(fl);			
			ltlBottonsPanel.setBackground(Utils.backgroundColor);	
		}else{
			ltlBottonsPanel.removeAll();
		}
		
		String toolTipText="<html><i>Adds LTL formula to the current set of LTL properties.<br>" +
		"Example: G(oscil(MA,3)) </i></html>";	
	    CustomToolTipButton b1=new CustomToolTipButton("Add",toolTipText); 
	    b1.setName("Add");
	    b1.setActionCommand("addLTL");
	    b1.addActionListener(listener);
		b1.setBalloonToolTipVisible(false);
		
	    ltlBottonsPanel.add(b1);
		
	}
	
			
	/**
	 * Creates the bottom panel with the model checking results
	 * 
	 * */
	private void constructBottomPanel() {		
		
		if(resultsPanel==null){
			resultsPanel=new JPanel();			 
			resultsPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createLineBorder(Color.black),"Results"));
			resultsPanel.setLayout(new BorderLayout());	
			resultsPanel.setBackground(Utils.backgroundColor);		
			resultsPanel.setName("LTL Model Checking");
			tarea=new JTextArea();
			tarea.setText(ltlModel.getManipulationResults());		
			tarea.setSelectionColor(Color.WHITE);
			tarea.setBackground(Utils.backgroundColor);
			tarea.setEditable(false);
			resultsPanel.add(tarea,BorderLayout.CENTER);
			JButton clear=new JButton("Clear Screen");
			clear.addActionListener(new ActionListener(){

				public void actionPerformed(ActionEvent e) {					
					ltlModel.setManipulationResults("");				
				}});
			JButton save=new JButton("Save");
			save.setActionCommand("saveLTLModelChecking");
			save.addActionListener(listener);
			JPanel resultButtonsP=new JPanel();
			resultButtonsP.setLayout(new FlowLayout());
			resultButtonsP.setBackground(Utils.backgroundColor);
			resultButtonsP.add(clear);
			resultButtonsP.add(save);
			resultsPanel.add(resultButtonsP,BorderLayout.LINE_END);
			Utils.modern.setBorderThickness(3);
			Utils.modern.enableAntiAliasing(true);
		}
		
	}
    
	/**
	 * Refreshes the central panel. Removes all LTL content, and re-creates it with the current ltlModel data.
	 * 
	 * */
	public void refresh(){
		
		constructGUI();
		
	
	}
	
	/**
	 * Return a reference of the output results from the work with the specifications.
	 * */
	public JTextArea getTarea() {
		return tarea;
	}
		


	public JFrame getParentFrame() {
		return parentFrame;
	}


	public void setParentFrame(JFrame parentFrame) {
		this.parentFrame = parentFrame;
	}

	public void appendResults(String r){
	
		ltlModel.setManipulationResults(tarea.getText()+r);		
	}
}


