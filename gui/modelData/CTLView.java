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
import net.java.balloontip.BalloonTip;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.customComponents.DeleteButton;
import fr.inria.contraintes.biocham.customComponents.UniversalScrollPane;
import fr.inria.contraintes.biocham.modelData.CTLController.DeleteProperty;
import fr.inria.contraintes.biocham.utils.Utils;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFormattedTextField;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SpringLayout;


/**
 * The view displays the CTL data and executes corresponding functions on view clicking events.
 * 
 * There is only one FIX view per model that is attached to the ModelEditorPanel, and there can be any number of popup generated views.
 * The FIX view and the generated popup view share the same model. 
 * There is only one popup view per time. When the frame is being closed, its disposed, and when generated again, new view is created as a model representation.  
 * The model contains the list of specifications and the generated results from their manipulation.
 *  
 * @author Dragana Jovanovska
 *  
 * */
public class CTLView extends PopupSplitPane{

	
	static String elementName="Boolean Temporal properties";	
	
	CTLModel ctlModel;
	
	CustomToolTipButton add;	
	CTLController listener;
	DeleteProperty deleteListener;
	BalloonTip bTip=null;
	SpringLayout topLeftPanelLayout;
	JTextArea tarea;		
	
	TopLeftCTLPanelFirst topLeftPanel;
	JPanel topRightPanel,resultsPanel;	
	JPanel ctlSpecPanel,ctlBottonsPanel;	
	JPanel resultsButtonsPanel;	
	JPanel topLeftAll;	
	JFrame parentFrame;


	/**
	 * Constructor that memorizes the reference to the CTL data,
	 * and calls a method the constructs the panel's graphical view. 
	 * */
	public CTLView(JFrame parent, CTLModel model){
		super(JSplitPane.VERTICAL_SPLIT);		
		parentFrame=parent;
		ctlModel=model;	
		listener=new CTLController(ctlModel,this);			
		deleteListener=listener.getDeleteListener();		
		ctlModel.getViews().add(this);
		constructGUI();
		Utils.debugMsg("\n\n ***** Number of views="+ctlModel.getViews().size());
		
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
		JSplitPane upCTL=new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,new UniversalScrollPane(topLeftAll),new UniversalScrollPane(topRightPanel));		
		upCTL.setResizeWeight(0.2);
		upCTL.setDividerLocation(0.5);
		upCTL.setDividerSize(1);
		upCTL.setContinuousLayout(true);
		setOrientation(JSplitPane.VERTICAL_SPLIT);
		setLeftComponent(upCTL);	
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
		topLeftAll=new JPanel(new GridLayout(2,1));		
		topLeftPanel=new TopLeftCTLPanelFirst(listener);
		topLeftAll.add(topLeftPanel);
		topLeftAll.add(new TopLeftCTLPanelSecond(listener));
	}
	
	/**
	 * Creates top right panel with the model CTL Specifications
	 * 
	 * */
	private void constructTopRightPanel() {		
		if(topRightPanel==null){
			topRightPanel=new JPanel(new BorderLayout());
			topRightPanel.setBackground(Utils.backgroundColor);
		}else{
			topRightPanel.removeAll();
		}	
		constructCtlSpecPanel();
		constructButtonsPanel();
		
		topRightPanel.add(ctlSpecPanel,BorderLayout.CENTER);		
		topRightPanel.add(ctlBottonsPanel,BorderLayout.NORTH);
	}
	
	/**
	 * Constructs the CTL Specifications panel that lists all the CTL currently defined in the model.
	 * 
	 * */
	private void constructCtlSpecPanel() {
		
		if(ctlSpecPanel==null){
			ctlSpecPanel=new JPanel(new SpringLayout());
			ctlSpecPanel.setBackground(Utils.backgroundColor);
		}else{
			ctlSpecPanel.removeAll();
		}		
		int size=ctlModel.getSpecifications().size();
		
		if(size>0){
			
			SpringLayout layout=(SpringLayout)ctlSpecPanel.getLayout();
			int northBefore=20;
			String parent=null;
			for(int i=0; i<ctlModel.getSpecifications().size();i++){				
				parent=ctlModel.getSpecifications().get(i);
				JFormattedTextField tf=new JFormattedTextField();
				tf.setColumns(30);
				tf.setName(parent);
				tf.setValue(parent);
				tf.setHorizontalAlignment(JTextField.LEFT);
				tf.setEditable(false);				
		    	DeleteButton but2=new DeleteButton();
		    	but2.setName(parent);		    
		    	but2.addMouseListener(deleteListener);    
		    			    			    	
		    	ctlSpecPanel.add(tf);
		    	ctlSpecPanel.add(but2);	 		    	
		    	
		    	layout.putConstraint(SpringLayout.WEST, tf, 5, SpringLayout.WEST, ctlSpecPanel);
				layout.putConstraint(SpringLayout.NORTH, tf, northBefore,SpringLayout.NORTH, ctlSpecPanel);				
				layout.putConstraint(SpringLayout.WEST, but2, 10, SpringLayout.EAST, tf);
				layout.putConstraint(SpringLayout.NORTH, but2, northBefore,SpringLayout.NORTH, ctlSpecPanel);
							
		    	northBefore+=30;
			}	
			ctlSpecPanel.setPreferredSize(new Dimension(50,northBefore+60));		
			this.revalidate();
		}
	}
	
	/**
	 * Creates the buttons for adding a new CTL property,generating automatically some or adding automatically generated ones, revise/learn/reduce rules.
	 * 
	 * */
	private void constructButtonsPanel() {
		
		
		if(ctlBottonsPanel==null){
			FlowLayout fl=new FlowLayout(FlowLayout.LEFT);		
			ctlBottonsPanel=new JPanel(fl);			
			ctlBottonsPanel.setBackground(Utils.backgroundColor);	
		}else{
			ctlBottonsPanel.removeAll();
		}
		
		String toolTipText="<html><i>Add CTL temporal properties to the current set of CTL properties.<br>" +
    		"Example: Ei(reachable(!(MB))) </i></html>";		
	    CustomToolTipButton b1=new CustomToolTipButton("Add",toolTipText); 
	    b1.setName("Add");
	    b1.setActionCommand("addCTL");
	    b1.addActionListener(listener);
		b1.setBalloonToolTipVisible(false);
		
	    toolTipText="<html><i>Adds to the current specification those simple CTL properties <br>" +
	    					  "(reachable, oscil, steady, checkpoint for each molecule) that <br>" +
	    					  "are true in the model.</i></html>";
	    CustomToolTipButton b2=new CustomToolTipButton("Add generated CTL",toolTipText); 
	    b2.setName("AddGenCTL");
	    b2.setActionCommand("addGenCTL");
	    b2.addActionListener(listener);		
	    b2.setBalloonToolTipVisible(false);
	  
	    toolTipText="<html><i>Automatically generates a set of all CTL properties of some simple pattern that are true in the model.</i></html>";
	    CustomToolTipButton b02=new CustomToolTipButton("Generate CTL",toolTipText); 
	    b02.setName("genCTL");
	    b02.setActionCommand("genCTL");
	    b02.addActionListener(listener);		
	    b02.setBalloonToolTipVisible(false);
	    
	  
		if(ctlSpecPanel.getComponentCount()>0){			
		
			 ctlBottonsPanel.add(b1);
			 ctlBottonsPanel.add(b02);
			 ctlBottonsPanel.add(b2);			
			
		}else{			
			
			ctlBottonsPanel.add(b1);
			ctlBottonsPanel.add(b02);
			ctlBottonsPanel.add(b2);		
							 
		}		
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
			resultsPanel.setName("CTL Model Checking");
			tarea=new JTextArea();
			tarea.setText(ctlModel.getManipulationResults());		
			tarea.setSelectionColor(Color.WHITE);
			tarea.setBackground(Utils.backgroundColor);
			tarea.setEditable(false);
			resultsPanel.add(tarea,BorderLayout.CENTER);
			JButton clear=new JButton("Clear Screen");
			clear.addActionListener(new ActionListener(){

				public void actionPerformed(ActionEvent e) {					
					ctlModel.setManipulationResults("");				
				}});
			JButton save=new JButton("Save");
			save.setActionCommand("saveCTLModelChecking");
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
	 * Refreshes the central panel. Removes all CTL content, and re-creates it with the current ctlModel data.
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
		ctlModel.setManipulationResults(tarea.getText()+r);		
	}
}

