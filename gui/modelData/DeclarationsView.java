package fr.inria.contraintes.biocham.modelData;

import net.java.balloontip.BalloonTip;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.customComponents.DeleteButton;
import fr.inria.contraintes.biocham.customComponents.ModifyButton;
import fr.inria.contraintes.biocham.modelData.DeclarationsController.DeleteDeclaration;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.Utils;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.TreeSet;

import javax.swing.JFormattedTextField;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.Spring;
import javax.swing.SpringLayout;



/**
 * The view displays the data and executes on view clicking events.
 * 
 * */
public class DeclarationsView extends ParametersPanel{

	
	DeclarationsModel declarationsModel;
	static String elementName="Declarations";	
	CustomToolTipButton add;	
	JPanel buttonsPanel,centralPanel;	
	DeclarationsController listener;
	MouseListener mouseListener;
	DeleteDeclaration deleteListener;
	BalloonTip bTip=null;	
	KeyListener keyListener;
	JFrame parentFrame;
	
	/**
	 * Constructor that memorizes the reference to the declarations data,
	 * and calls a method the constructs the panel's graphical view. 
	 * */
	public DeclarationsView(JFrame parent, DeclarationsModel model){
		super(elementName);		
		parentFrame=parent;
		declarationsModel=model;	
		listener=new DeclarationsController(declarationsModel,this);
		mouseListener=listener.getMouseListener();		
		deleteListener=listener.getDeleteListener();
		declarationsModel.getViews().add(this);
		constructGUI();		
		keyListener=listener.getKeyListener();
	}

	/**
	 * Constructs the panel. It contains 2 subpanels: 
	 * One at the bottom that contains the event buttons (buttonsPanel), and 
	 * the second at the center (centarPanel) that lists the declarations in the model.
	 * */
	private void constructGUI(){
		
			
		centralPanel=new JPanel(new SpringLayout());
		buttonsPanel=new JPanel(new FlowLayout(FlowLayout.LEADING));		
		centralPanel.setBackground(Utils.backgroundColor);
		buttonsPanel.setBackground(Utils.backgroundColor);
		constructButtonsPanel();
		constructDeclarationsPanel();
		add(centralPanel,BorderLayout.CENTER);
		add(buttonsPanel,BorderLayout.SOUTH);
		
	}
	
	/**
	 * Creates the buttons for adding a new declaration,deleting, etc.
	 * 
	 * */
	private void constructButtonsPanel() {		
		
		
		String toolTipText="<html><i>Associate to a molecule a set of sites which can be phosphorylated.<br>Example: name=MA where MA represents a molecule name, and<br>&nbsp &nbsp &nbsp &nbsp &nbsp &nbsp &nbsp  value=p1,p2,p3, where pi represent phosphorylation sites.</i></html>";
		add=new CustomToolTipButton("Add",toolTipText);	
		add.setBalloonToolTipVisible(false);
		add.setName("Add");
		add.setActionCommand("addDeclaration");		
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
	    buttonsPanel.add(refreshButton);		
	}
	
	/**
	 * Refreshes the central panel. Removes all declarations content, and re-creates it with the current declarationsModel data.
	 * 
	 * */
	public void refresh(){
		centralPanel.removeAll();
		constructDeclarationsPanel();
		repaint();
	
	}
	
	/**
	 * Constructs the central panel that lists all the declarations currently defined in the model.
	 * 
	 * */
	private void constructDeclarationsPanel() {
		
		int size=declarationsModel.getDeclarations().size();		
		if(size>0){
			
			SpringLayout layout=(SpringLayout) centralPanel.getLayout();
			int northBefore=20;					
			for(String parent: declarationsModel.getDeclarations().keySet()){//new TreeSet<String>()){				
				
				JLabel l = new JLabel(parent);
				l.setName(parent);	
				JFormattedTextField tf=new JFormattedTextField();
				tf.setColumns(20);
				tf.setName(parent);
				tf.setValue(declarationsModel.getDeclarations().get(parent));
				tf.setHorizontalAlignment(JTextField.LEFT);
				tf.setEditable(false);
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
				tf.addMouseListener(mouseListener);
				tf.addKeyListener(keyListener);				
				l.setLabelFor(tf);
				ModifyButton but1=new ModifyButton();				
		    	but1.setName(parent);
		    	but1.addMouseListener(mouseListener);
		    	DeleteButton but2=new DeleteButton();
		    	but2.setName(parent);		    
		    	but2.addMouseListener(deleteListener);    
		    			    	
		    	centralPanel.add(l);
				centralPanel.add(tf);
		    	centralPanel.add(but1);
		    	centralPanel.add(but2);	 
		    	
		    	
		    	layout.putConstraint(SpringLayout.WEST, l, 5, SpringLayout.WEST, centralPanel);
				layout.putConstraint(SpringLayout.NORTH, l, northBefore,SpringLayout.NORTH, centralPanel);				
				layout.putConstraint(SpringLayout.NORTH, tf, northBefore,SpringLayout.NORTH, centralPanel);		
				layout.putConstraint(SpringLayout.WEST, tf, 5, SpringLayout.EAST, l);
				layout.putConstraint(SpringLayout.WEST, but1, 10, SpringLayout.EAST, tf);
				layout.putConstraint(SpringLayout.NORTH, but1, northBefore,SpringLayout.NORTH, centralPanel);
				layout.putConstraint(SpringLayout.WEST, but2, 5, SpringLayout.EAST, but1);
				layout.putConstraint(SpringLayout.NORTH, but2, northBefore,SpringLayout.NORTH, centralPanel);
				 
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
