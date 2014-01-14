package fr.inria.contraintes.biocham.modelData;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.customComponents.DeleteButton;
import fr.inria.contraintes.biocham.customComponents.ModifyButton;
import fr.inria.contraintes.biocham.modelData.RulesController.DeleteRule;
import fr.inria.contraintes.biocham.modelData.RulesController.ModifyRule;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.Utils;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTree;
import javax.swing.SpringLayout;




/**
 * The view displays the data and executes on view clicking events.
 * 
 * */
public class RulesView extends ParametersPanel{

	
	RulesModel rulesModel;
	static String elementName="Reaction Rules";	
	CustomToolTipButton add,show, export, deleteAll, rGEditor,rgraph;	
	JPanel buttonsPanel,centralPanel;	
	RulesController listener;
	ModifyRule modifyListener;
	DeleteRule deleteListener;
	JFrame parentFrame;
	ArrayList<BiochamDynamicTree> trees=new ArrayList<BiochamDynamicTree>();
	SpringLayout layout=new SpringLayout();
	static int gcounter=0; 
	/**
	 * Constructor that memorizes the reference to the rules data,
	 * and calls a method the constructs the panel's graphical view. 
	 *
	 */
	public RulesView(JFrame parent,RulesModel model){
		super(elementName);	
		parentFrame=parent;
		Utils.debugMsg(parentFrame.getClass().toString());
		rulesModel=model;	
		listener=new RulesController(rulesModel,this);
		modifyListener=listener.getModifyListener();
		deleteListener=listener.getDeleteListener();
		rulesModel.getViews().add(this);
		constructGUI();
		
	}

	/**
	 * Constructs the panel. It contains 2 subpanels: 
	 * One at the bottom that contains the event buttons (buttonsPanel), and 
	 * the second at the center (centarPanel) that lists the reaction rules in the model.
	 * */
	private void constructGUI(){
		
			
		centralPanel=new JPanel(layout);
		buttonsPanel=new JPanel(new FlowLayout(FlowLayout.LEADING));		
		centralPanel.setBackground(Utils.backgroundColor);
		buttonsPanel.setBackground(Utils.backgroundColor);
		constructButtonsPanel();
		constructRulesPanel();
		add(centralPanel,BorderLayout.CENTER);
		add(buttonsPanel,BorderLayout.SOUTH);
		
	}
	
	/**
	 * Constructs the central panel that lists all the reaction rules currently present in the model.
	 * */
	private void constructRulesPanel() {
		
		
		int size=rulesModel.getRules().size();
		
		if(size>0){
			
			gcounter++;
			
			int counter=0;
			int northBefore=20;
			BiochamDynamicTree tree;					
			//GridBagConstraints c=null;
			
			for(String parent: new ArrayList<String>(rulesModel.getRules().keySet())){				
				
				tree=new BiochamDynamicTree(parent);						
				tree.setName(parent);
				tree.tree.setName(parent);		
				for(int k=0;k<rulesModel.getRules().get(parent).size();k++){
					tree.addRuleObject(rulesModel.getRules().get(parent).get(k));	
					//northBefore+=50;
				}
				collapseAll(tree.tree);
				
				ModifyButton but1=new ModifyButton();				
		    	but1.setName(parent);
		    	but1.addMouseListener(modifyListener);		    	
		    	DeleteButton b2=new DeleteButton();
		    	b2.setName(parent);		    
		    	b2.addMouseListener(deleteListener);    
		    	
		    	centralPanel.add(but1);
		    	centralPanel.add(b2);
		    	centralPanel.add(tree.tree);
		    	
		    	layout.putConstraint(SpringLayout.WEST,but1, 5,SpringLayout.WEST,centralPanel);
		    	layout.putConstraint(SpringLayout.WEST,b2, 15,SpringLayout.EAST,but1);
		    	layout.putConstraint(SpringLayout.WEST,tree.tree, 15,SpringLayout.EAST,b2);
		    	
		    	layout.putConstraint(SpringLayout.NORTH,but1, northBefore,SpringLayout.NORTH,centralPanel);
		    	layout.putConstraint(SpringLayout.NORTH,b2, 0,SpringLayout.NORTH,but1);
		    	layout.putConstraint(SpringLayout.NORTH,tree.tree, 0,SpringLayout.NORTH,but1);
		    	/*c=new GridBagConstraints();		    		    
		    	c.anchor=GridBagConstraints.WEST;
		    	c.gridx=0;		    	
		    	c.gridy=counter;			    	
		    	c.insets=new Insets(20, 10, 10, 0);
		    	c.weightx = 1.0;
		    	c.weighty = 1.0;
		    	centralPanel.add(but1,c);		    			    	
		    	c=new GridBagConstraints();		    		    
		    	c.anchor=GridBagConstraints.WEST;		    	
		    	c.gridx=0;		    	
		    	c.gridy=counter;	
		    	c.insets=new Insets(20,40,10,30);
		    	c.weightx = 1.0;
		    	c.weighty = 1.0;
		    	centralPanel.add(b2,c);	
		    	c=new GridBagConstraints();		    		    
		    	c.anchor=GridBagConstraints.WEST;
		    	c.gridx=0;		    	
		    	c.gridy=counter;	
		    	c.insets=new Insets(20,70,10,0);
		    	c.weightx = 1.0;
		    	c.weighty = 1.0;
		    	c.fill = GridBagConstraints.BOTH;
		    	centralPanel.add(tree.tree,c);	*/
				
		    	trees.add(tree);
		    	northBefore+=70;
				counter++;
			}					
			tree=null;
			centralPanel.setPreferredSize(new Dimension(50,northBefore+60));
			this.revalidate();
		
			
		}		
	}
	
	 public void collapseAll(JTree tree) {
		    int row = tree.getRowCount() - 1;
		    while (row >= 0) {
		      tree.collapseRow(row);
		      row--;
		    }
	 }

	
	/**
	 * Creates the buttons for adding a new reaction rule, showing and exporting 
	 * kinetics, deleting all the rules, and launching the graphical reaction editor.
	 * 
	 * */
	private void constructButtonsPanel() {
		
		String toolTipText="<html><i>Add a reaction rule to the current set of rules if any.</i></html>";
		add=new CustomToolTipButton("Add",toolTipText);	
		add.setBalloonToolTipVisible(false);
		add.setName("Add");
		add.setActionCommand("addRule");
		
		
		toolTipText="<html><i>Shows kinetics in a separate window.</i></html>";
	    show=new CustomToolTipButton("Show Kinetics",toolTipText);	
	    show.setBalloonToolTipVisible(false);
	    show.setName("showKinetics");
	    show.setActionCommand("showKinetics");
	   
	    
	    toolTipText="<html><i>Export the kinetics to a file.</i></html>";
	    export=new CustomToolTipButton("Export Kinetics",toolTipText);	
	    export.setBalloonToolTipVisible(false);
	    export.setName("exportKinetics");
	    export.setActionCommand("exportKinetics");
	    
	    
	    toolTipText="<html><i>Deletes all the reactions' rules.</i></html>";
	    deleteAll=new CustomToolTipButton("Delete All",toolTipText);	
	    deleteAll.setBalloonToolTipVisible(false);
	    deleteAll.setName("deleteAll");
	    deleteAll.setActionCommand("deleteAll");
	   
	    
	    toolTipText="<html><i>Launches the graphical reaction editor in a separate window.</i></html>";
	    rGEditor=new CustomToolTipButton("Graphical Editor",toolTipText);	
	    rGEditor.setBalloonToolTipVisible(false);
	    rGEditor.setName("launchGraphicalEditor");
	    rGEditor.setActionCommand("rge");
	    
	    toolTipText="<html><i>Visualizes the graph resulting from an export_dot with no options, using both Graphviz and gv.</i></html>";
	    rgraph=new CustomToolTipButton("Reactions Graph",toolTipText);	
	    rgraph.setBalloonToolTipVisible(false);
	    rgraph.setName("rgraph");
	    rgraph.setActionCommand("rgraph");
	  
	    rGEditor.addActionListener(listener);
	    deleteAll.addActionListener(listener);
	    export.addActionListener(listener);
	    show.addActionListener(listener);
	    add.addActionListener(listener);
	    rgraph.addActionListener(listener);
	    
	    JLabel refreshButton=new JLabel();
	    refreshButton.setIcon(Icons.icons.get("Refresh3.png"));		
	    refreshButton.setName("refresh");	    
	    refreshButton.setText("Screen Refresh");
	    refreshButton.setForeground(Utils.refreshedColor);
	    refreshButton.setToolTipText("Click to Refresh the Screen");
	    refreshButton.addMouseListener(new MouseAdapter(){
	    	public void mouseClicked(MouseEvent me) { 
	           //repaint();
	    		refresh();
	        }
	    });
	    
	    buttonsPanel.add(add);
	    buttonsPanel.add(show);
	    buttonsPanel.add(export);
	    buttonsPanel.add(deleteAll);
	    buttonsPanel.add(rGEditor);
	    buttonsPanel.add(rgraph);
	    buttonsPanel.add(refreshButton);
		
	}

	/**
	 * Refreshes the central panel. Removes all rules content, and re-creates it with the current rulesModel data.
	 * 
	 * */
	public void refresh(){
		centralPanel.removeAll();
		constructRulesPanel();
		repaint();
		Utils.debugMsg(gcounter);
	
	}
	

	
		
	public JPanel getCentralPanel() {
		return centralPanel;
	}
	
	public RulesModel getRulesModel() {
		return rulesModel;
	}
	public void setRulesModel(RulesModel rulesModel) {
		this.rulesModel = rulesModel;
	}
	public JFrame getParentFrame() {
		Utils.debugMsg(parentFrame.toString());
		return parentFrame;
	}
	public void setParentFrame(JFrame parentFrame) {
		this.parentFrame = parentFrame;
	}
	
}
