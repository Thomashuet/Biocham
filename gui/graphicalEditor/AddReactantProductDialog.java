package fr.inria.contraintes.biocham.graphicalEditor;

import fr.inria.contraintes.biocham.customComponents.GradientPanel;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.SpringLayout;


public class AddReactantProductDialog extends JDialog implements ActionListener{

	
	 JComboBox reactionsComboBox;
	 BiochamGraph graphInstance;
	 String chosenReactant="";
	 int stoichCoeff=0;
	 
	 
	 /**
	  * 
	  * part of the Reaction Graph Editor(RGE).  
	  * Dialog that opens on right click on a reaction on the RGE or from the RGE's toolbar
	  * Adding reactant, product or modulator to an existing reaction on the reaction graph
	  * Gives the possibility for adding, to choose from the existing molecules or to create a new molecule.
	  * 
	  * */
	 public AddReactantProductDialog(String title,BiochamGraph graph){
		
		 
		super(JOptionPane.getFrameForComponent(graph.getBiochamModel().getGraphEditor()),true);		
		setTitle(title);
		((java.awt.Frame)this.getOwner()).setIconImage(Icons.images.get("water_molecule-icon5.png"));
		graphInstance=graph;
		
		Container contents=this.getContentPane();
		contents.setLayout(new BorderLayout());
		GradientPanel main=new GradientPanel();
		SpringLayout layout=new SpringLayout();
	    main.setLayout(layout);

	    JLabel imageLabel=new JLabel(Icons.icons.get("molecule2.png"+0.1));
	    main.add(imageLabel);
	    
	    layout.putConstraint(SpringLayout.WEST, imageLabel, 20,SpringLayout.WEST, main);
		layout.putConstraint(SpringLayout.NORTH, imageLabel, 20, SpringLayout.NORTH, main);
		int maxWidth=0;
	
		String[] list=GraphUtilities.getUniqueMoleculesNames(graph);
		for(int i=0;i<list.length;i++){			
				if(maxWidth<list[i].length()){
					maxWidth=list[i].length();
				}			
		}    
	    reactionsComboBox=new JComboBox(list);
	    reactionsComboBox.setBackground(Utils.backgroundColor.brighter());
	    reactionsComboBox.setSelectedIndex(0);
	    main.add(reactionsComboBox);
	    
	    layout.putConstraint(SpringLayout.WEST, reactionsComboBox, 10,SpringLayout.EAST, imageLabel);
		layout.putConstraint(SpringLayout.NORTH, reactionsComboBox, 0, SpringLayout.NORTH, imageLabel);
		
	    JButton newRP=new JButton("Create New");
	    newRP.setActionCommand("new");
	    newRP.addActionListener(this);
	    main.add(newRP);
	    
	    layout.putConstraint(SpringLayout.WEST, newRP, 10,SpringLayout.EAST, reactionsComboBox);
		layout.putConstraint(SpringLayout.NORTH, newRP, 0, SpringLayout.NORTH, imageLabel);
	    
	    JButton ok=new JButton("OK");
		ok.setActionCommand("OK");
		ok.addActionListener(new ActionListener() {
			
			/**
			 *  Button "Ok" for executing the addition of the molecule to the reaction.
			 *  
			 **/
			public void actionPerformed(ActionEvent event) {
			
			setChosenReactant(reactionsComboBox.getSelectedItem().toString());
			String newValue= (String)JOptionPane.showInputDialog(JOptionPane.getFrameForComponent(graphInstance.getBiochamModel().getGraphEditor()),"Enter coefficient of stoichiometry (OPTIONAL):\n","Coefficient of Stoichiometry"); 
			if(newValue!=null && newValue!=""){
				try{
					setStoichCoeff(Integer.valueOf(newValue));
				}catch(Exception e){
					setStoichCoeff(1);
				}
			}
			setVisible(false);
			dispose();
		}});
		JButton cancel=new JButton("CANCEL");
		cancel.setActionCommand("CANCEL");		
		cancel.addActionListener(new ActionListener() {
			/**
			 *  Button "Cancel" that gives up of the adding and closes the dialog.
			 *  
			 **/
			public void actionPerformed(ActionEvent event) { 
			setVisible(false);
			dispose();
			
		}});
	    
		main.add(ok);
		main.add(cancel);
		
		layout.putConstraint(SpringLayout.WEST, ok, maxWidth,SpringLayout.WEST, reactionsComboBox);
		layout.putConstraint(SpringLayout.NORTH, ok, 60, SpringLayout.SOUTH, imageLabel);
		layout.putConstraint(SpringLayout.WEST, cancel, 10,SpringLayout.EAST, ok);
		layout.putConstraint(SpringLayout.NORTH, cancel, 0, SpringLayout.NORTH, ok);
		
		setLocationByPlatform(true);
		contents.add(main,BorderLayout.CENTER);		
		setResizable(true);
		setSize(new Dimension(maxWidth+250, 180));
		setVisible(true);
		list=null;
		
	}

	 /**
	  * Actions perfomed when the dialog's buttons are being clicked.
	  * 
	  * Button "New" for creating a new molecule
	  * 	  
	  * */
	public void actionPerformed(ActionEvent e) {
	
		if(e.getSource() instanceof JButton){

			/**
			 * Creating a new molecule for the purposes of adding a reactant, product or a modulator to a reaction
			 * */
			if(e.getActionCommand().equals("new")){

			
				final DialogNewMolecule newMol=new DialogNewMolecule(null,graphInstance);
				graphInstance.clearSelection();
				SwingWorker sw=new SwingWorker(){
					NewEntity nmd;
					@Override
					public Object construct() {						
						return null;
					}
					@Override
					public void finished() {
						nmd=newMol.getNewEntityData();		
						if(nmd!=null){
							reactionsComboBox.addItem(nmd.getName());
							reactionsComboBox.setSelectedItem(nmd.getName());
							BiochamCellData data1=new BiochamEntityData(graphInstance);
							((BiochamEntityData)data1).setMoleculeState(nmd.getState());
							((BiochamEntityData)data1).setName(nmd.getName());
							((BiochamEntityData)data1).setModificationSites(nmd.getModificationSites());
							((BiochamEntityData)data1).setCompartment(nmd.getLocation());
							((BiochamEntityData)data1).setInitialConcentration(nmd.getInitialConcentration());													
							((BiochamEntityData)data1).setMultimerCardinality(nmd.getMultimerCoef());
							if(nmd.getType().contains("Macromolecule")){
								graphInstance.getGraphLayoutCache().insert(new EMacromoleculeCell(data1));
							}else if(nmd.getType().contains("Nucleic")){
								graphInstance.getGraphLayoutCache().insert(new ENucleicAcidFeatureCell(data1));
							}else if(nmd.getType().contains("Complex")){
								graphInstance.getGraphLayoutCache().insert(new EComplexCell(data1));
							}
							graphInstance.refresh();							
						}
						nmd=null;
					}};
				sw.start();
				
			}
		}
		
	}

	/**
	 * Returns the chosen molecule for adding.
	 * */
	public String getChosenReactant() {	
		return chosenReactant;
	}
	/**
	 * Sets a molecule for adding. 
	 * */
	public void setChosenReactant(String chosenReactant) {
		this.chosenReactant = chosenReactant;
	}
	/**
	 * Returns the stoichoimetry coeficient for the adding of the chosen molecule.
	 * */
	public int getStoichCoeff() {
		return stoichCoeff;
	}
	/**
	 * Sets the stoichoimetry coeficient for the adding of the chosen molecule.
	 * */
	public void setStoichCoeff(int stoichCoeff) {
		this.stoichCoeff = stoichCoeff;
	}
}
