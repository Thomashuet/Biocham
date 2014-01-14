package fr.inria.contraintes.biocham.graphicalEditor;

import net.java.balloontip.BalloonTip;

import org.jgraph.graph.DefaultGraphCell;

import fr.inria.contraintes.biocham.customComponents.GradientPanel;
import fr.inria.contraintes.biocham.utils.Icons;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.HashSet;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.SpringLayout;





public class MoleculeEditorWindow extends JDialog{

	
	
	
	BiochamEntityData molData;
	JLabel name, state, initConc, compartment;
	JTextField initConcTF, nameTF, stateTF,locationTF;
	JComboBox stateCB, locationCB;
	JButton ok,cancel;	
	
	public MoleculeEditorWindow(JFrame parent, BiochamEntityData data,BiochamGraph graph){
		
		super(parent,true);
		molData=data;
		this.setTitle(data.getName());
		((java.awt.Frame)this.getOwner()).setIconImage(Icons.images.get("greenIcon.png"));		
		createUI(graph,data);
		setSize(new Dimension(350, 250));	   
		setResizable(false);
		setLocationRelativeTo(parent);		
		setVisible(true);	
	}

	
	

	public void createUI(BiochamGraph graph,BiochamEntityData data) {

		
		Container contents=this.getContentPane();
		contents.setLayout(new BorderLayout());		
		GradientPanel panel=new GradientPanel();		
		contents.add(panel,BorderLayout.CENTER);			
		SpringLayout layout=new SpringLayout();
		panel.setLayout(layout);
		
		name=new JLabel("Name: "); 
		nameTF=new JTextField(data.getName(),15);
		nameTF.setEditable(false);
		
		initConc=new JLabel("Initial State: ");
		initConcTF=new JTextField(data.getInitialConcentration(),15);		
		initConcTF.setEditable(false);
		
		compartment=new JLabel("Location: ");
		/*ECompartmentCell[] cells=GraphUtilities.getAllCompartments(graph);
		HashSet<String> locs=new HashSet<String>();
		for(int i=0; i<cells.length;i++){
				locs.add(((BiochamCompartmentData)cells[i].getUserObject()).getCompartmentName());
		}
		cells=null;
		String[] locations=new String[locs.size()+1];
		locations[0]="Default";
		Object[] array=locs.toArray();
		for(int i=0;i<locs.size();i++){
			locations[i+1]=array[i].toString();
		}*/	
		String comp=data.getCompartment();
		if(comp.startsWith("::")){
			comp=comp.substring(comp.lastIndexOf(":")+1);
		}
		if(comp==""){
			comp="Default";
		}
		locationTF=new JTextField(comp);
		locationTF.setColumns(15);
		locationTF.setEditable(false);
		/*locationCB=new JComboBox(locations);
		locationCB.setBackground(Color.white);
		if(data.getName().contains("::")){
			data.setCompartment(data.getName().substring(data.getName().indexOf("::")+2));
		}
		locationCB.setSelectedItem(data.getCompartment());		
		
		*/
		String stateL=data.getMoleculeState();
		if(data.getMoleculeState()==MoleculeState.MODIFIED && data.getModificationSites()!=null){
			stateL+=": "+data.getModificationSites();
		}
		stateTF=new JTextField(stateL);
		stateTF.setColumns(15);
		stateTF.setEditable(false);
		state=new JLabel("State: "); 
		/*stateCB=new JComboBox(BiochamGraph.states);
		stateCB.setBackground(Color.white);
		stateCB.setSelectedItem(data.getMoleculeState());*/
				
		panel.add(name);
		panel.add(nameTF);
		layout.putConstraint(SpringLayout.WEST, name, 50,SpringLayout.WEST, panel);
		layout.putConstraint(SpringLayout.NORTH, name, 40, SpringLayout.NORTH, panel);		
		layout.putConstraint(SpringLayout.NORTH, nameTF, 0, SpringLayout.NORTH, name);
		
		panel.add(initConc);
		panel.add(initConcTF);
		layout.putConstraint(SpringLayout.WEST, initConc, 50,SpringLayout.WEST, panel);
		layout.putConstraint(SpringLayout.NORTH, initConc, 15, SpringLayout.SOUTH, name);
		layout.putConstraint(SpringLayout.WEST, initConcTF, 5,SpringLayout.EAST, initConc);
		layout.putConstraint(SpringLayout.NORTH, initConcTF, 0, SpringLayout.NORTH, initConc);
		
		layout.putConstraint(SpringLayout.WEST, nameTF, 0,SpringLayout.WEST, initConcTF);
		
		panel.add(compartment);
		panel.add(locationTF);
		layout.putConstraint(SpringLayout.WEST, compartment, 50,SpringLayout.WEST, panel);
		layout.putConstraint(SpringLayout.NORTH, compartment, 15, SpringLayout.SOUTH, initConc);
		layout.putConstraint(SpringLayout.WEST, locationTF, 0,SpringLayout.WEST, initConcTF);
		layout.putConstraint(SpringLayout.NORTH, locationTF, 0, SpringLayout.NORTH, compartment);
		
		panel.add(state);
		panel.add(stateTF);
		
		layout.putConstraint(SpringLayout.WEST, state, 50,SpringLayout.WEST, panel);
		layout.putConstraint(SpringLayout.NORTH, state, 15, SpringLayout.SOUTH, compartment);
		layout.putConstraint(SpringLayout.WEST, stateTF, 0,SpringLayout.WEST, initConcTF);
		layout.putConstraint(SpringLayout.NORTH, stateTF, 0, SpringLayout.NORTH, state);
		
		ok=new JButton("OK");
		ok.setActionCommand("OK");
		ok.addActionListener(new ActionListener() {public void actionPerformed(ActionEvent event) { windowAction(event);}});			
		cancel=new JButton("Cancel");
		cancel.setActionCommand("CANCEL");
		cancel.addActionListener(new ActionListener() {public void actionPerformed(ActionEvent event) { windowAction(event);}});		
		panel.add(ok);
		panel.add(cancel);
		layout.putConstraint(SpringLayout.WEST, ok, 100-ok.getWidth(),SpringLayout.WEST, panel);
		layout.putConstraint(SpringLayout.NORTH, ok, 35, SpringLayout.SOUTH, stateTF);
		layout.putConstraint(SpringLayout.WEST, cancel, 10,SpringLayout.EAST, ok);
		layout.putConstraint(SpringLayout.NORTH, cancel, 0, SpringLayout.NORTH, ok);	
		
		
	   
		
	}
	
	
	protected void windowAction(ActionEvent event) {
		
		
		 boolean closeWindow = false;
	     String cmd = null;	    
	      
	      if (event != null) {
	    	  
	         if (event instanceof ActionEvent) {
	        	 
	            cmd = ((ActionEvent)event).getActionCommand();
	            
	         } else {
	        	 
	            cmd = event.toString();
	            
	         }
	      }
	      
	      if (cmd == null) {
	         // do nothing
	      } else if (cmd.equals("CANCEL")) {
	    	  
	    	  closeWindow = true;
	    	  
	      } else if (cmd.equals("OK")) {
		
		/*	molData.setName(nameTF.getText().trim());
			molData.setInitialConcentration(initConcTF.getText());
			molData.setCompartment(locationCB.getSelectedItem().toString());
			molData.setMoleculeState(stateCB.getSelectedItem().toString());*/
			closeWindow = true;
			
	      }
		
	      if (closeWindow) {
		         setVisible(false);
		         dispose();
	      }
	}




	/*public BiochamEntityData getMolData() {
		return molData;
	}
	public void setMolData(BiochamEntityData molData) {
		this.molData = molData;
	}*/
}
