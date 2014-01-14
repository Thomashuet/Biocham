package fr.inria.contraintes.biocham.modelData;

import net.java.balloontip.BalloonTip;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.customComponents.DeleteButton;
import fr.inria.contraintes.biocham.customComponents.ModifyButton;
import fr.inria.contraintes.biocham.modelData.MacrosController.DeleteMacro;
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
public class MacrosView extends ParametersPanel{

		
		MacrosModel macrosModel;
		static String elementName="Macros";	
		CustomToolTipButton add;	
		JPanel buttonsPanel,centralPanel;	
		MacrosController listener;
		MouseListener mouseListener;
		DeleteMacro deleteListener;
		BalloonTip bTip=null;	
		KeyListener keyListener;
		JFrame parentFrame;
		
		/**
		 * Constructor that memorizes the reference to the Macros data,
		 * and calls a method the constructs the panel's graphical view. 
		 * */
		public MacrosView(JFrame parent, MacrosModel model){
			super(elementName);		
			parentFrame=parent;
			macrosModel=model;	
			listener=new MacrosController(macrosModel,this);
			mouseListener=listener.getMouseListener();		
			deleteListener=listener.getDeleteListener();
			keyListener=listener.getKeyListener();
			macrosModel.getViews().add(this);
			constructGUI();
		
		}
		
		/**
		 * Constructs the panel. It contains 2 subpanels: 
		 * One at the bottom that contains the event buttons (buttonsPanel), and 
		 * the second at the center (centarPanel) that lists the Macros in the model.
		 * */
		private void constructGUI(){
			
				
			centralPanel=new JPanel(new SpringLayout());
			buttonsPanel=new JPanel(new FlowLayout(FlowLayout.LEADING));		
			centralPanel.setBackground(Utils.backgroundColor);
			buttonsPanel.setBackground(Utils.backgroundColor);
			constructButtonsPanel();
			constructMacrosPanel();
			add(centralPanel,BorderLayout.CENTER);
			add(buttonsPanel,BorderLayout.SOUTH);
			
		}
		
		/**
		 * Creates the buttons for adding a new Macro,deleting, etc.
		 * 
		 * */
		private void constructButtonsPanel() {		
			
			String toolTipText="<html><i>Declare a parameter.<br> Example: parameter(k1,4.0675).</i></html>";
			add=new CustomToolTipButton("Add",toolTipText);	
			add.setBalloonToolTipVisible(false);
			add.setName("Add");
			add.setActionCommand("addMacro");		
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
		 * Refreshes the central panel. Removes all Macros content, and re-creates it with the current macrosModel data.
		 * 
		 * */
		public void refresh(){
			centralPanel.removeAll();
			constructMacrosPanel();
			repaint();
		
		}
		/**
		 * Constructs the central panel that lists all the Macros currently defined in the model.
		 * 
		 * */
		private void constructMacrosPanel() {
			
			int size=macrosModel.getMacros().size();		
			if(size>0){
				
				SpringLayout layout=(SpringLayout) centralPanel.getLayout();
				int northBefore=20;					
				for(String parent: macrosModel.getMacros().keySet()){// new TreeSet<String>(){				
					
					JFormattedTextField tf=new JFormattedTextField();
					tf.setColumns(30);
					tf.setName(parent);
					tf.setValue(macrosModel.getMacros().get(parent));
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
					JLabel l = new JLabel(parent);
					l.setName(parent);	
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
