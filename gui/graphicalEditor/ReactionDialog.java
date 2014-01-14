package fr.inria.contraintes.biocham.graphicalEditor;


import net.java.balloontip.BalloonTip;

import org.jgraph.graph.DefaultGraphCell;

import fr.inria.contraintes.biocham.customComponents.CustomComboBox;
import fr.inria.contraintes.biocham.customComponents.CustomMultiSelectComboBox;
import fr.inria.contraintes.biocham.customComponents.GradientPanel;
import fr.inria.contraintes.biocham.customComponents.JLabel2D;
import fr.inria.contraintes.biocham.customComponents.SimpleComboBox;
import fr.inria.contraintes.biocham.dialogs.DialogAddSpecification;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GradientPaint;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.StringTokenizer;
import java.util.UUID;
import java.util.Vector;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.Spring;
import javax.swing.SpringLayout;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;





public class ReactionDialog extends JFrame implements ActionListener{

	
	String dialogName;
	SpringLayout layout, layout2;
	Spring maxSpring;
	Container contents;
	JLabel2D title;
	JLabel reactants, products, kinetics, reversibility, modulator, reversibilityModulator;
	ArrayList<JComponent[]> sections; // list of sections.
	// Section correspond to one of the rection properties
	// CustomMultiSelectComboBox nEntitiesComboBox;
	JComboBox reactionsComboBox; //nEntitiesComboBox,oneEntityComboBox;
	JComboBox productsComboBox;
	JComboBox modulatorComboBox;
	JComboBox modulatorComboBox2;
	JTextField reactantsSelected, productsSelected, kineticValue, modulatorSelected1,modulatorSelected2;
	
	//????????????????????????
	JTextField coeffReactants, coeffProducts, coeffModulator1, coeffModulator2;
	
	JButton newReactant, newProduct, newModulator1, newModulator2;
	JRadioButton reversYes, reversNo, singleRevers, doubleRevers;
	JCheckBox reversModulator;
	static BiochamGraph graph;
	ButtonGroup group1, group2;
	JPanel main,buttons;
	GradientPanel intro;
	String[] listWithSourceSink, listWithoutSourceSink = null;
	String selectedIndividuallyR="",selectedIndividuallyP="",selectedIndividuallyM1="",selectedIndividuallyM2="";
	public final ReactionDialog instance;
	static ArrayList<String> addedItems;
	
	
	public ReactionDialog(BiochamGraph graph,JInternalFrame parent, String dialogName){
			
		//super(JOptionPane.getFrameForComponent(BiochamGraphEditorDesktop.graphDesktop),true);	
		this.graph=graph;
		this.dialogName=dialogName;		
		this.setIconImage(Icons.images.get("kreversi.png"));
	    createUI();
	    setLocationRelativeTo(parent);
	    setVisible(true);	   
	    instance=this;
	    addedItems=new ArrayList<String>();
	}
	
	
	
	private void createUI() {

		setTitle(dialogName);		
		contents= getContentPane();
		contents.setLayout(new BorderLayout());
	    
	    
	    buttons=new JPanel();
	    buttons.setBackground(Utils.backgroundColor);	
	    
		intro=new GradientPanel();
		FlowLayout fl=new FlowLayout();
		if(dialogName.contains("State")){
			fl.setHgap(50);
		}else{
			fl.setHgap(60);
		}
		
		fl.setVgap(20);
		fl.setAlignment(FlowLayout.LEFT);
		intro.setLayout(fl);
		
		main=new JPanel();
		layout=new SpringLayout();
	    main.setLayout(layout);
	    maxSpring=Spring.constant(30);
	    main.setBackground(Utils.backgroundColor);
	    
		group1 = new ButtonGroup();
		group2 = new ButtonGroup();
		
		title =new JLabel2D(this.dialogName);
		title.setFont(new Font(title.getFont().getFontName(),Font.PLAIN,16));
		title.setOutlineColor(Color.GRAY);
		title.setEffectIndex(JLabel2D.EFFECT_GRADIENT);	
		GradientPaint gp = new GradientPaint(0, 0, Utils.backgroundColor.brighter(), 0, 0, Utils.backgroundColor.darker(), true);
		title.setGradient(gp);
		JLabel icon=new JLabel(Icons.icons.get("atom1.png"+0.5));
		intro.add(icon);
		intro.add(new JLabel());
		intro.add(title);	
		intro.setSize(680,200);		
  	  	sections=new ArrayList<JComponent[]>(); //5 sections.......
		
		
		//1. Reactants
  	  	
		reactants = new JLabel("Reactants: ");
		reactionsComboBox=getReactantsComboBox(this.dialogName);	
		reactionsComboBox.setName("reactants");
		
		
		reactantsSelected=new JTextField(" Selected Reactants ",40);
		newReactant=new JButton("Create New");
		newReactant.setActionCommand("newReactant");
		newReactant.addActionListener(this);
		sections.add(new JComponent[]{reactants,reactionsComboBox,reactantsSelected,newReactant});
		main.add(reactants);
		main.add(reactionsComboBox);
		main.add(reactantsSelected);
		main.add(newReactant);
		
		//SpringLayout 1
		layout.putConstraint(SpringLayout.WEST, reactants, 60,SpringLayout.WEST, main);
		layout.putConstraint(SpringLayout.NORTH, reactants, 30, SpringLayout.NORTH, main);
		layout.putConstraint(SpringLayout.WEST, reactionsComboBox, 10,SpringLayout.EAST, reactants);
		layout.putConstraint(SpringLayout.NORTH, reactionsComboBox, 0, SpringLayout.NORTH, reactants);
		layout.putConstraint(SpringLayout.WEST, newReactant, 10,SpringLayout.EAST, reactionsComboBox);
		layout.putConstraint(SpringLayout.NORTH, newReactant, 0, SpringLayout.NORTH, reactionsComboBox);
		layout.putConstraint(SpringLayout.WEST, reactantsSelected, 0,SpringLayout.WEST, reactants);
		layout.putConstraint(SpringLayout.NORTH, reactantsSelected, 10, SpringLayout.SOUTH, reactants);
    	  	  
    	  
		//2. Products
		products = new JLabel("Products: ");
		productsComboBox=getProductsComboBox(this.dialogName);
		productsComboBox.setName("products");
		
		productsSelected=new JTextField(" Selected Products ",40);
		newProduct=new JButton("Create New");
		newProduct.setActionCommand("newProduct");
		newProduct.addActionListener(this);
		sections.add(new JComponent[]{products,productsComboBox,productsSelected,newProduct});
		main.add(products);
		main.add(productsComboBox);
		main.add(productsSelected);
		main.add(newProduct);
		
		//Springlayout 2
		layout.putConstraint(SpringLayout.WEST, products, 0,SpringLayout.WEST, reactants);
		layout.putConstraint(SpringLayout.NORTH, products, 30, SpringLayout.SOUTH, reactantsSelected);
		layout.putConstraint(SpringLayout.WEST, productsComboBox, 10,SpringLayout.EAST, products);
		layout.putConstraint(SpringLayout.NORTH, productsComboBox, 0, SpringLayout.NORTH, products);
		layout.putConstraint(SpringLayout.WEST, newProduct, 10,SpringLayout.EAST, productsComboBox);
		layout.putConstraint(SpringLayout.NORTH, newProduct, 0, SpringLayout.NORTH, productsComboBox);
		layout.putConstraint(SpringLayout.WEST, productsSelected, 0,SpringLayout.WEST, reactants);
		layout.putConstraint(SpringLayout.NORTH, productsSelected, 10, SpringLayout.SOUTH, products);
		
		if(dialogName.equals("Association")){
			products.setEnabled(false);
			productsComboBox.setEnabled(false);
			productsSelected.setEnabled(false);
			newProduct.setEnabled(false);
		}
		
		//3. Kinetics
		kinetics = new JLabel("Kinetics: ");
		kineticValue=new JTextField(" Help Compose ",34);
		kineticValue.addMouseListener(new MouseAdapter(){
  		  public void mouseClicked(MouseEvent e) {
				if(e.getClickCount()==2){
					DialogAddSpecification params=new DialogAddSpecification(instance,graph.getBiochamModel() ,"Kinetics",main);					
					String value=params.getFormula();
					kineticValue.setText(value);	
				}	    				
			}
	  });
		sections.add(new JComponent[]{kinetics,kineticValue});
		main.add(kinetics);
		main.add(kineticValue);		
				
		//Springlayout 3
		layout.putConstraint(SpringLayout.WEST, kinetics, 0,SpringLayout.WEST, reactants);
		layout.putConstraint(SpringLayout.NORTH, kinetics, 30, SpringLayout.SOUTH, productsSelected);
		layout.putConstraint(SpringLayout.WEST, kineticValue, 10,SpringLayout.EAST, kinetics);
		layout.putConstraint(SpringLayout.NORTH, kineticValue, 0, SpringLayout.NORTH, kinetics);		
		
		
		
		//4. Reversibility
		reversibility = new JLabel("Reversibility: ");
		reversYes=new JRadioButton("Yes");
		reversYes.setBackground(Utils.backgroundColor);
		group1.add(reversYes);
		reversYes.setSelected(false);
		reversYes.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				JRadioButton cb=(JRadioButton)e.getSource();
				if (cb.isSelected()){
					reversNo.setSelected(false);
					if(dialogName.contains("State")){
						singleRevers.setEnabled(true);
						singleRevers.setSelected(true);
						doubleRevers.setEnabled(true);
						doubleRevers.setSelected(false);
					}else{
						((JCheckBox)sections.get(4)[4]).setEnabled(true);
						((JCheckBox)sections.get(4)[4]).setSelected(false);
					}
					
				}else{
					
					reversNo.setSelected(true);
					//if(dialogName.contains("State")){
						singleRevers.setEnabled(false);
						doubleRevers.setEnabled(false);
						((JCheckBox)sections.get(4)[4]).setEnabled(false);
						((JCheckBox)sections.get(4)[4]).setSelected(false);
						sections.get(4)[5].setEnabled(false);
						sections.get(4)[6].setEnabled(false);
						sections.get(4)[7].setEnabled(false);
					//}
				}
				
			
		}});
		reversNo=new JRadioButton("No");
		reversNo.setSelected(true);
		reversNo.setBackground(Utils.backgroundColor);
		reversNo.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				JRadioButton cb=(JRadioButton)e.getSource();
				if (cb.isSelected()){				

					reversNo.setSelected(true);
					//if(dialogName.contains("State")){
						singleRevers.setEnabled(false);
						doubleRevers.setEnabled(false);
					//}
					((JCheckBox)sections.get(4)[4]).setEnabled(false);
					((JCheckBox)sections.get(4)[4]).setSelected(false);
					sections.get(4)[5].setEnabled(false);
					sections.get(4)[6].setEnabled(false);
					sections.get(4)[7].setEnabled(false);
				}else{
					reversYes.setSelected(false);	
					((JCheckBox)sections.get(4)[4]).setEnabled(true);
					((JCheckBox)sections.get(4)[4]).setSelected(false);
					
				}			
		}});
		group1.add(reversNo);
		singleRevers=new JRadioButton("Single");
		singleRevers.setSelected(true);
		singleRevers.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				JRadioButton cb=(JRadioButton)e.getSource();
				if (cb.isSelected()){
					if(dialogName.contains("State")){
						doubleRevers.setSelected(false);
					}
					//disabling reversibility modulator
					((JCheckBox)sections.get(4)[4]).setEnabled(false);
					((JCheckBox)sections.get(4)[4]).setSelected(false);
					sections.get(4)[5].setEnabled(false);
					sections.get(4)[6].setEnabled(false);
					sections.get(4)[7].setEnabled(false);
				}else{
					if(dialogName.contains("State")){
						singleRevers.setSelected(false);
					}
					//enabling reversibility modulator
					((JCheckBox)sections.get(4)[4]).setEnabled(true);
					((JCheckBox)sections.get(4)[4]).setSelected(false);	
				}			
		}});
		doubleRevers=new JRadioButton("Double");
		doubleRevers.setSelected(false);
		doubleRevers.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				JRadioButton cb=(JRadioButton)e.getSource();
				if (cb.isSelected()){
					if(dialogName.contains("State")){
						singleRevers.setSelected(false);
					}
					//enabling reversibility modulator
					((JCheckBox)sections.get(4)[4]).setEnabled(true);
					((JCheckBox)sections.get(4)[4]).setSelected(false);
				}else{
					if(dialogName.contains("State")){
						singleRevers.setSelected(true);	
						doubleRevers.setSelected(false);
					}
					//disabling reversibility modulator
					((JCheckBox)sections.get(4)[4]).setEnabled(false);
					((JCheckBox)sections.get(4)[4]).setSelected(false);
					sections.get(4)[5].setEnabled(false);
					sections.get(4)[6].setEnabled(false);
					sections.get(4)[7].setEnabled(false);					
				}
				if(!cb.isEnabled()){
					((JCheckBox)sections.get(4)[4]).setEnabled(false);
					((JCheckBox)sections.get(4)[4]).setSelected(false);
					sections.get(4)[5].setEnabled(false);
					sections.get(4)[6].setEnabled(false);
					sections.get(4)[7].setEnabled(false);	
				}
		}});
		singleRevers.setBackground(Utils.backgroundColor);
		doubleRevers.setBackground(Utils.backgroundColor);
		group2.add(singleRevers);
		group2.add(doubleRevers);
		
		sections.add(new JComponent[]{reversibility,reversYes,reversNo,singleRevers,doubleRevers});
		main.add(reversibility);
		main.add(reversYes);
		main.add(reversNo);
		main.add(singleRevers);
		main.add(doubleRevers);
		
		
		//Springlayout 4
		layout.putConstraint(SpringLayout.WEST, reversibility, 0,SpringLayout.WEST, reactants);
		layout.putConstraint(SpringLayout.NORTH, reversibility, 30, SpringLayout.SOUTH, kinetics);
		layout.putConstraint(SpringLayout.WEST, reversYes, 10,SpringLayout.EAST, reversibility);
		layout.putConstraint(SpringLayout.NORTH, reversYes, 0, SpringLayout.NORTH, reversibility);
		layout.putConstraint(SpringLayout.WEST, reversNo, 10,SpringLayout.EAST, reversYes);
		layout.putConstraint(SpringLayout.NORTH, reversNo, 0, SpringLayout.NORTH, reversYes);
		layout.putConstraint(SpringLayout.WEST, singleRevers, 20,SpringLayout.WEST, reactants);
		layout.putConstraint(SpringLayout.NORTH, singleRevers, 10, SpringLayout.SOUTH, reversibility);
		layout.putConstraint(SpringLayout.WEST, doubleRevers, 10,SpringLayout.EAST, singleRevers);
		layout.putConstraint(SpringLayout.NORTH, doubleRevers, 0, SpringLayout.NORTH, singleRevers);
		
		
		//5. Modulators: uni/bi-directional
		modulator = new JLabel("Modulator: ");
		modulatorComboBox=getModulatorsComboBox(this.dialogName);
		modulatorComboBox.setName("modulator1");
		modulatorSelected1=new JTextField(" Modulator Selected ",40);
		newModulator1=new JButton("Create New");
		newModulator1.setActionCommand("newModulator");
		newModulator1.addActionListener(this);
		reversModulator=new JCheckBox("Oposite direction modulator:");
		reversModulator.setBackground(Utils.backgroundColor);
		reversModulator.setSelected(false);
		reversModulator.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
				JCheckBox cb=(JCheckBox)e.getSource();
				if (cb.isSelected()){
					sections.get(4)[5].setEnabled(true);
					sections.get(4)[6].setEnabled(true);
					sections.get(4)[7].setEnabled(true);					
				}else{	
					sections.get(4)[5].setEnabled(false);
					sections.get(4)[6].setEnabled(false);
					sections.get(4)[7].setEnabled(false);					
				}			
				
			}});
		modulatorComboBox2=getModulatorsComboBox(this.dialogName);
		modulatorComboBox2.setName("modulator2");
		modulatorSelected2=new JTextField(" Selected Modulator 2",40);
		newModulator2=new JButton("Create New");	
		newModulator2.setActionCommand("newModulator");
		newModulator2.addActionListener(this);
		sections.add(new JComponent[]{modulator,modulatorComboBox,modulatorSelected1,newModulator1,reversModulator,modulatorComboBox2,modulatorSelected2,newModulator2});		
		main.add(modulator);
		main.add(modulatorComboBox);
		main.add(modulatorSelected1);
		main.add(newModulator1);
		main.add(reversModulator);
		main.add(modulatorComboBox2);
		main.add(modulatorSelected2);
		main.add(newModulator2);
		
		//Springlayout 5
		layout.putConstraint(SpringLayout.WEST, modulator, 0,SpringLayout.WEST, reactants);
		layout.putConstraint(SpringLayout.NORTH, modulator, 30, SpringLayout.SOUTH, singleRevers);
		layout.putConstraint(SpringLayout.WEST, modulatorComboBox, 10,SpringLayout.EAST, modulator);
		layout.putConstraint(SpringLayout.NORTH, modulatorComboBox, 0, SpringLayout.NORTH, modulator);
		layout.putConstraint(SpringLayout.WEST, newModulator1, 10,SpringLayout.EAST, modulatorComboBox);
		layout.putConstraint(SpringLayout.NORTH, newModulator1, 0, SpringLayout.NORTH, modulatorComboBox);
		layout.putConstraint(SpringLayout.WEST, modulatorSelected1, 0,SpringLayout.WEST, reactants);
		layout.putConstraint(SpringLayout.NORTH, modulatorSelected1, 10, SpringLayout.SOUTH, modulator);
		layout.putConstraint(SpringLayout.WEST, reversModulator, 0,SpringLayout.WEST, reactants);
		layout.putConstraint(SpringLayout.NORTH, reversModulator, 10, SpringLayout.SOUTH, modulatorSelected1);
		layout.putConstraint(SpringLayout.WEST, modulatorComboBox2, 10,SpringLayout.EAST, reversModulator);
		layout.putConstraint(SpringLayout.NORTH, modulatorComboBox2, 0, SpringLayout.NORTH, reversModulator);
		layout.putConstraint(SpringLayout.WEST, newModulator2, 10,SpringLayout.EAST, modulatorComboBox2);
		layout.putConstraint(SpringLayout.NORTH, newModulator2, 0, SpringLayout.NORTH, modulatorComboBox2);
		layout.putConstraint(SpringLayout.WEST, modulatorSelected2,0,SpringLayout.WEST, reactants);
		layout.putConstraint(SpringLayout.NORTH, modulatorSelected2, 10, SpringLayout.SOUTH, reversModulator);
						
		
		JButton ok=new JButton("OK");
		ok.setActionCommand("OK");
		ok.addActionListener(new ActionListener() {public void actionPerformed(ActionEvent event) { windowAction(event);}});
		JButton cancel=new JButton("CANCEL");
		cancel.setActionCommand("CANCEL");
		cancel.addActionListener(new ActionListener() {public void actionPerformed(ActionEvent event) { windowAction(event);}});
		FlowLayout lay=new FlowLayout();
		lay.setHgap(40);
		lay.setVgap(20);
		buttons.setLayout(lay);
		buttons.add(ok);
		buttons.add(cancel);
		buttons.setSize(680,200);
		main.setPreferredSize(new Dimension(500,450));	 
		
		setSectionsState(this.dialogName);		
		setTextFieldsEditable(false);
		
		contents.add(intro,BorderLayout.NORTH);
		JScrollPane sp=new JScrollPane(main);
		sp.setAutoscrolls(true);
		contents.add(sp,BorderLayout.CENTER);
		contents.add(buttons,BorderLayout.SOUTH);		
		setResizable(true);
		setSize(new Dimension(600, 550));
		
	}

	private void setTextFieldsEditable(boolean b) {
		reactantsSelected.setEditable(b);
		productsSelected.setEditable(b);
		modulatorSelected1.setEditable(b);
		modulatorSelected2.setEditable(b);
		
	}

	private void setSectionsState(String n) {
		
		if(n.contains("Transition")){
			
			//enable all
			for(int i=0;i<sections.size();i++){
				for(int j=0;j<sections.get(i).length;j++){
					sections.get(i)[j].setEnabled(true);
				}
			}
			//disable corresponding, if No reversibility is chosen!
			if(((JRadioButton)sections.get(3)[2]).isSelected()){
				((JRadioButton)sections.get(3)[3]).setEnabled(false);//single
				((JRadioButton)sections.get(3)[4]).setEnabled(false);//double
				// disabling reversibility modulator
				((JCheckBox)sections.get(4)[4]).setEnabled(false);
				sections.get(4)[5].setEnabled(false);
				sections.get(4)[6].setEnabled(false);
				sections.get(4)[7].setEnabled(false);
			}
			
		}else if(n.contains("Association") || n.contains("Dissociation")){
			
			
			for(int i=0;i<sections.size();i++){
				
				//disable reversibility section
				if(i==3){
					for(int j=0;j<sections.get(i).length;j++){
						if(j==3 || j==4){
							sections.get(i)[j].setEnabled(false);
						}else{
							sections.get(i)[j].setEnabled(true);
						}
					}
				//enable all other sections
				}else if(i==1){
					for(int j=0;j<sections.get(i).length;j++){
						sections.get(i)[j].setEnabled(false);
					}
				}else{					
					for(int j=0;j<sections.get(i).length;j++){
						sections.get(i)[j].setEnabled(true);
					}
				}
			}
			//disable reversible Modulator section
			((JCheckBox)sections.get(4)[4]).setEnabled(false);
			sections.get(4)[5].setEnabled(false);
			sections.get(4)[6].setEnabled(false);
			sections.get(4)[7].setEnabled(false);
			
			
		}		
		/*if(n.contains("Transition")){
		
			//enable all
			for(int i=0;i<sections.size();i++){
				for(int j=0;j<sections.get(i).length;j++){
					sections.get(i)[j].setEnabled(true);
				}
			}
			//disable corresponding, if No reversibility is chosen!
			if(((JRadioButton)sections.get(3)[2]).isSelected()){
				((JRadioButton)sections.get(3)[3]).setEnabled(false);//single
				((JRadioButton)sections.get(3)[4]).setEnabled(false);//double
				// disabling reversibility modulator
				((JCheckBox)sections.get(4)[4]).setEnabled(false);
				sections.get(4)[5].setEnabled(false);
				sections.get(4)[6].setEnabled(false);
				sections.get(4)[7].setEnabled(false);
			}
			
		}else if(n.contains("Association") || n.contains("Dissociation")){
			
			
			for(int i=0;i<sections.size();i++){
				
				//disable reversibility section
				if(i==3 || i==1){
					for(int j=0;j<sections.get(i).length;j++){
						sections.get(i)[j].setEnabled(false);
					}
				//enable all other sections
				}else{					
					for(int j=0;j<sections.get(i).length;j++){
						sections.get(i)[j].setEnabled(true);
					}
				}
			}
			//disable reversible Modulator section
			((JCheckBox)sections.get(4)[4]).setEnabled(false);
			sections.get(4)[5].setEnabled(false);
			sections.get(4)[6].setEnabled(false);
			sections.get(4)[7].setEnabled(false);
			
			
		}		*/
		
	}

	private JComboBox getReactantsComboBox(String n) {
		
		JComboBox cb=null;	 
				
		if(n.contains("Transition")){
			cb=getMultiSelectComboBox(getListWithSourceSink(),true);
			
		}else if(n.contains("Association")){
			cb=getMultiSelectComboBox(getListWithoutSourceSink(),false);
			
		}else if(n.contains("Dissociation")){
			cb=getSimpleComboBox(getListWithoutSourceSink());
		}
				 
		return cb;	
	}
		
	private JComboBox getSimpleComboBox(String[] reactantsList) {
		JComboBox cb=new SimpleComboBox(reactantsList);
		
		BalloonTip bTip=new BalloonTip(cb,"Don't forget to press ENTER to apply the selection.",Utils.modern,BalloonTip.Orientation.RIGHT_BELOW,BalloonTip.AttachLocation.SOUTHEAST,20,10,false);
		bTip.setText("Don't forget to press ENTER to apply the selection.");
		bTip.setIcon(Icons.icons.get("flag_blue.png"));
		bTip.setIconTextGap(10);	
		bTip.setVisible(false);
		bTip.enableClickToHide(true);	
		cb.getModel().setSelectedItem("");		
		((SimpleComboBox)cb).setBTip(bTip);
		cb.addPopupMenuListener(new ComboBoxPopupListener());	
		cb.addKeyListener(new KeyAdapter(){
			public void keyPressed(KeyEvent e) {

				if(e.getSource() instanceof SimpleComboBox){
					
					SimpleComboBox cb=(SimpleComboBox)e.getSource();				
					if(e.getKeyCode()==KeyEvent.VK_ENTER){	
						
						setTextFieldsEditable(true);
	    				if(cb.getName().equals("reactants")){
	    					reactantsSelected.setText(cb.getSelectedItem().toString());
	    				}else if(cb.getName().equals("products")){
	    					productsSelected.setText(cb.getSelectedItem().toString());
	    				}else if(cb.getName().equals("modulator1")){
	    					modulatorSelected1.setText(cb.getSelectedItem().toString());
	    				}else if(cb.getName().equals("modulator2")){
	    					modulatorSelected2.setText(cb.getSelectedItem().toString());
	    				}
	    				setTextFieldsEditable(false);
	    				cb.getBTip().setVisible(false);
	    				cb.getBTip().setEnabled(false);	
	    				cb.setPopupVisible(false);
					}					
				}			
			}
		});
		
		return cb;
	}

	public JComboBox getMultiSelectComboBox(String[] reactantsList, boolean b) {
		
		JComboBox cb=(new CustomComboBox(reactantsList,b)).getMultiSelectComboBox();
		BalloonTip bTip=new BalloonTip(cb,"Don't forget to press ENTER to apply the selection.",Utils.modern,BalloonTip.Orientation.RIGHT_BELOW,BalloonTip.AttachLocation.SOUTHEAST,20,10,false);
		bTip.setText("Don't forget to press ENTER to apply the selection.");
		bTip.setIcon(Icons.icons.get("flag_blue.png"));
		bTip.setIconTextGap(10);	
		bTip.setVisible(false);
		bTip.enableClickToHide(true);	
		((CustomMultiSelectComboBox)cb).setBTip(bTip);
		Dimension d = cb.getPreferredSize();
		int maxWidth=d.width;
		for(int i=0;i<reactantsList.length;i++){
			if(reactantsList[i].length()>maxWidth){
				maxWidth=reactantsList[i].length();
			}
		}
		((CustomMultiSelectComboBox)cb).setPreferredSize(new Dimension(130, d.height));
		((CustomMultiSelectComboBox)cb).setPopupWidth(maxWidth+40);      
		cb.addPopupMenuListener(new ComboBoxPopupListener());	
		cb.addKeyListener(new ComboboxkeyListener());
		return cb;
		
		
	}

	private JComboBox getProductsComboBox(String n) {
		
		
		if(n.contains("Transition")){			
			
			return getMultiSelectComboBox(getListWithSourceSink(),true);
			
		}else if(n.contains("Association")){
			
			return getSimpleComboBox(getListWithoutSourceSink());
			
		}else if(n.contains("Dissociation")){
			
			return getMultiSelectComboBox(getListWithoutSourceSink(),false);
			
		}else
			return null;
	}	
		
	private JComboBox getModulatorsComboBox(String n) {
				
		return getMultiSelectComboBox(getListWithoutSourceSink(),false);//getSimpleComboBox(getListWithoutSourceSink());
			
	}	
	
	private String[] getListWithSourceSink(){
		if(listWithSourceSink==null){
			String[] cells=GraphUtilities.getUniqueMoleculesNames(graph);
			listWithSourceSink=new String[cells.length+1];		
			for(int i=0;i<cells.length;i++){
				listWithSourceSink[i]=cells[i];
			}			
			listWithSourceSink[cells.length]="Source/Sink";			
		}
		return listWithSourceSink;
	}
	
	public String[] getListWithoutSourceSink(){
		if(listWithoutSourceSink==null){
			//Object[] cells=BiochamGraph.getUniqueVerticesOnly(graph.getModel(),graph.getRoots());
			listWithoutSourceSink=GraphUtilities.getUniqueMoleculesNames(graph);//new String[cells.length];
			
		}
		return listWithoutSourceSink;
	}

	public void addItemToComboLists(String item){
		productsComboBox.addItem(item);													
		reactionsComboBox.addItem(item);
		modulatorComboBox.addItem(item);
		modulatorComboBox2.addItem(item);
		addedItems.add(item);
	}
	public void removeItemFromComboLists(String item){
		
		removeItems(productsComboBox,item);												
		removeItems(reactionsComboBox,item);
		removeItems(modulatorComboBox,item);
		removeItems(modulatorComboBox2,item);
	}

	
	private static void removeItems(JComboBox comboBox, String item) {
		for(int i=0;i<comboBox.getItemCount();i++){
		
			if(comboBox.getItemAt(i).toString().equals(item)){
				comboBox.removeItemAt(i);
				comboBox.repaint();
			}
		}		
	}



	public void actionPerformed(ActionEvent e) {
		
		if(e.getSource() instanceof JButton){
			final String cmd=((JButton)e.getSource()).getActionCommand();
			if(cmd.contains("new")){
				
				graph.clearSelection();
				final DialogNewMolecule newMol=new DialogNewMolecule(this,graph);
				graph.clearSelection();
				SwingWorker sw=new SwingWorker(){
					NewEntity nmd;
					@Override
					public Object construct() {				
						//while(!newMol.isDisposed()){}
						return null;
					}
					@Override
					public void finished() {
						nmd=newMol.getNewEntityData();		
						if(nmd!=null){
							
							BiochamCellData data1=new BiochamEntityData(graph);
							((BiochamEntityData)data1).setMoleculeState(nmd.getState());
							((BiochamEntityData)data1).setRepresentingName(nmd.getRepresentingName());
							((BiochamEntityData)data1).setModificationSites(nmd.getModificationSites());
							((BiochamEntityData)data1).setMultimerCardinality(nmd.getMultimerCoef());
							
							((BiochamEntityData)data1).setName(nmd.getName().trim());
							((BiochamEntityData)data1).setCompartment(nmd.getLocation());
							((BiochamEntityData)data1).setInitialConcentration(nmd.getInitialConcentration());
							
													
							
							if(nmd.getType().contains("Macromolecule")){
								graph.getGraphLayoutCache().insert(new EMacromoleculeCell(data1));								
							}else if(nmd.getType().contains("Nucleic")){		
								graph.getGraphLayoutCache().insert(new ENucleicAcidFeatureCell(data1));
							}else if(nmd.getType().contains("Complex")){
								((BiochamEntityData)data1).setContainingMolecules(nmd.getContainingMolecules());
								graph.getGraphLayoutCache().insert(new EComplexCell(data1));
							}
							addItemToComboLists(nmd.getName().trim());
							String s="present("+((BiochamEntityData)data1).getName().trim()+","+((BiochamEntityData)data1).getInitialConcentration()+").\n";
							graph.getBiochamModel().sendToBiocham(s,"initConc");
							s=null;
							graph.refresh();							
						}
					}};
				sw.start();
				
			}
			
		}
	}
	
	 /**
	    * The user has selected an option.
	    * If actionCommand is an ActionEvent, getCommandString() is called,
	    * otherwise toString() is used to get the action command.
	    *
	    * @param actionCommand may be null
	    */
private void windowAction(Object actionCommand) {
	      boolean closeWindow = false;
	      String cmd = null;
	      if (actionCommand != null) {
	         if (actionCommand instanceof ActionEvent) {
	            cmd = ((ActionEvent)actionCommand).getActionCommand();
	         } else {
	            cmd = actionCommand.toString();
	         }
	      }
	      if (cmd == null) {
	         // do nothing
	      }else if(cmd.equals("OK")){
	    	  
	    	  //add the all added molecules...
	    	  Parser_SBGNRule2BiochamRule parser=new Parser_SBGNRule2BiochamRule(graph);
	    	  
	    	  parser.setReactants(reactantsSelected.getText().trim());
	    	  parser.setProducts(productsSelected.getText().trim());
	    	  parser.setReversibleSingle(reversYes.isSelected());
	    	  parser.setDoubleReversible(doubleRevers.isSelected());
	    	  if(!modulatorSelected1.getText().trim().contains("Selected")){
	    		  parser.setModulatorsSingle(modulatorSelected1.getText().trim());
	    	  }else{
	    		  parser.setModulatorsSingle(null);
	    	  }
	    	  if(!modulatorSelected2.getText().trim().contains("Selected")){
	    		  parser.setModulatorsDouble(modulatorSelected2.getText().trim());
	    	  }else{
	    		  parser.setModulatorsDouble(null);
	    	  }	    	
	    	 
	    	  parser.setReactionType(this.dialogName);
	    	  if(!(kineticValue.getText().length()<=0 || kineticValue.getText().equals(" ") || kineticValue.getText().contains("Help"))){
    			  parser.setKinetics(kineticValue.getText().trim());
    		  }/*else{
    			  parser.setKinetics("1.0");
    		  }*/
	    	  
	    	  String rule=parser.parse();
	    	  if(rule!=null){
	    		  Parser_SBGNRule2BiochamRule.setFromGraph(true);
	    		  GraphUtilities.addReactionFromGraphToModel(graph,rule);
	    	  }
	    	  /*if(Parser_SBGNRule2BiochamRule.ruleValidationPassed){
	    		  Parser_SBGNRule2BiochamRule
	    	  }*/
	    	  
	    	 /* String rule="";
	    	  String reversRuleProducts="",reversRuleReactants="",reversRuleModulator="";
	    	  
	    	  ArrayList<UUID> molecules=new ArrayList<UUID>();
	    	  BiochamEdgeData edgeData=new BiochamEdgeData(graph,molecules);
	    	  
	    	  
	    	  try{
	    		  String kinetics=kineticValue.getText().trim();		  
	    		
	    		  System.out.println(kinetics);
	    		  
	    		  if(!(kineticValue.getText().length()<=0 || kineticValue.getText().equals(" ") || kineticValue.getText().contains("Help"))){
	    			  rule=kinetics+" for ";
	    			  edgeData.setKinetics(kinetics);
	    		  }else{
	    			  edgeData.setKinetics("1.0");
	    		  }
	    	  }catch(Exception e){
	    		  edgeData.setKinetics("1.0");
	    		  
	    	  }
	    	  ArrayList<BiochamObject> reactants=new ArrayList<BiochamObject>();
	    	  ArrayList<BiochamObject> products=new ArrayList<BiochamObject>();
	    	  ArrayList<String> elems=new ArrayList<String>();	
	    	
	    	  String reacts=reactantsSelected.getText().trim();
	    	  StringTokenizer st=new StringTokenizer(reacts," ");
	    	  while(st.hasMoreTokens()){
	    		  elems.add(st.nextToken());
	    	  }
	    	  
	    	  for(int i=0;i<elems.size();i++){
	    		  if(elems.get(i).contains("(")){ //(Cdc2,3)
	    			
	    			  String name=elems.get(i).substring(1,elems.get(i).indexOf(";"));
	    			  int stoich=Integer.valueOf(elems.get(i).substring(elems.get(i).indexOf(";")+1,elems.get(i).length()-1));
	    			  if(name.equals("Source/Sink")){
	    				  BiochamEntityData data=new BiochamEntityData(graph);
	    				  data.setName("Source/Sink");
	    				  ESourceSink sourceSink=new ESourceSink(data);
	    				
	    				  graph.getGraphLayoutCache().insert(sourceSink);
	    				  graph.refresh();
	    				  reactants.add(new BiochamObject(sourceSink.addPort(),stoich,"Source/Sink",sourceSink));	
	    				  rule+="_";
	    			  }else{
	    				 // if(name.endsWith(")")){
	    				//	  name=name.substring(0,name.indexOf("("));
	    				 // }
	    				  DefaultGraphCell cell=GraphUtilities.getCellByName(graph,name);	    				  
	    				  Object port=cell.addPort();
	    				  BiochamEntityData dt=(BiochamEntityData)cell.getUserObject();
	    				  if(dt.getRepresentingName()==null){
	    					  dt.setRepresentingName(name);
	    				  }
	    				  if(stoich>1){
	    					  if(cell instanceof ENucleicAcidFeatureCell){
	    						  rule+=stoich+"*#"+name;
	    					  }else{
	    						  rule+=stoich+"*"+name;
	    					  }
	    					  
	    					 
	    					  reactants.add(new BiochamObject(port,stoich,name,cell));
	    					  
	    				  }else{
	    					  if(cell instanceof ENucleicAcidFeatureCell){
	    						  rule+="#"+name;
	    					  }else{
	    						  rule+=name;
	    					  }
	    					  
	    					  reactants.add(new BiochamObject(port,1,name,cell));
	    				  }
	    				  cell=null;
	    			  }
	    			  
	    		  }else{ //Cdc2
	    			
	    			  if(elems.get(i).equals("Source/Sink")){
	    				  BiochamEntityData data=new BiochamEntityData(graph);
	    				  data.setName("Source/Sink");
	    				  ESourceSink sourceSink=new ESourceSink(data);
	    			
	    				  graph.getGraphLayoutCache().insert(sourceSink);
	    				  reactants.add(new BiochamObject(sourceSink.addPort(),1,"Source/Sink",sourceSink));	
	    				  rule+="_";
	    				  data=null;
	    			  }else{
	    				  DefaultGraphCell cell=GraphUtilities.getCellByName(graph,elems.get(i));
	    				  if(cell!=null){
	    					  
	    					  BiochamEntityData dt=(BiochamEntityData)cell.getUserObject();
		    				  if(dt.getRepresentingName()==null){
		    					  dt.setRepresentingName(elems.get(i));
		    				  }
	    					  if(cell instanceof ENucleicAcidFeatureCell){
	    						  rule+="#"+dt.getRepresentingName();
	    					  }else{
	    						  rule+=dt.getRepresentingName();
	    					  }
	    					  
	    					  //BiochamEntityData ddt=(BiochamEntityData)cell.getUserObject();
	    					  reactants.add(new BiochamObject(cell.addPort(),dt.getMultimerCardinality(),dt.getRepresentingName(),cell));
	    					  dt=null;
	    				  }
	    				  cell=null;
	    			  }
	    			  
	    		  }
	    		  if(i<elems.size()-1){
	    			  rule+="+";
	    		  }
	    	  }
	    	  
	    	  
	    	  ArrayList<String> modulators1 = null;
	    	  reacts=modulatorSelected1.getText().trim();
	    	  if(!reacts.contains("Selected")){
	    		  if(reacts.substring(3).contains("(")){
	    			  //there are multiple modulators1............A+B+C=>A~{p1}+B+C syntax for biocham!!!!!!!!!!!!!!
	    			  StringTokenizer stz=new StringTokenizer(reacts," ");	
	    			  modulators1 =new ArrayList<String>(); 
		    		  while(stz.hasMoreTokens()){
		    			  String mod=stz.nextToken();
		    			  String nm=mod.substring(1,mod.indexOf(";"));
		    			  modulators1.add(nm);
		    			  DefaultGraphCell cell=GraphUtilities.getCellByName(graph,nm);
		    			  if(cell!=null){
		    				  BiochamEntityData dt=(BiochamEntityData)cell.getUserObject();
		    				  if(dt.getRepresentingName()==null){
		    					  dt.setRepresentingName(nm);
		    				  }
		    				  if(cell instanceof ENucleicAcidFeatureCell){
		    					  rule+="+#"+dt.getRepresentingName();
	    					  }else{
	    						  rule+="+"+dt.getRepresentingName();
	    					  }
		    			  }else{
		    				  rule+="+"+nm;
		    			  }    			  
		    			  cell=null;		    			  
		    			  mod=null;
		    			  nm=null;
		    		  }
		    		  if(reversYes.isSelected()){
	    				  rule+="<=>";
	    			  }else{
	    				  rule+="=>";
	    			  }
		    		  stz=null;
		    		  
	    		  }else{
	    			  //there is just one modulator, we can use this syntax....
	    			  String nm=reacts.substring(1,reacts.indexOf(";"));
	    			  int stoich=0;
	    			  try{
	    				  stoich=Integer.valueOf(reacts.substring(reacts.indexOf(";")+1,reacts.length()-1));
	    			  }catch(Exception e){}
	    			  DefaultGraphCell cell=GraphUtilities.getCellByName(graph,nm);
	    			  BiochamEntityData dt=(BiochamEntityData)cell.getUserObject();
    				
	    			  edgeData.setModulator1(cell);
	    			  
	    			  if(cell instanceof ENucleicAcidFeatureCell){
	    				  String geneName="#"+nm;
	    				  if(stoich>1){
	    					  geneName=stoich+"*"+nm;
	    					  dt.setStoichoimetryForReaction(Integer.toString(stoich));
	    					  cell.setUserObject(dt);
	    				  }
	    				  if(reversYes.isSelected()){
		    				  rule+="<=["+geneName+"]=>";
		    			  }else{
		    				  rule+="=["+geneName+"]=>";
		    			  }
					  }else{
						  String molname=nm;
	    				  if(stoich>1){
	    					  molname=stoich+"*"+nm;
	    					  dt.setStoichoimetryForReaction(Integer.toString(stoich));
	    					  cell.setUserObject(dt);
	    				  }
						  if(reversYes.isSelected()){
		    				  rule+="<=["+molname+"]=>";
		    			  }else{
		    				  rule+="=["+molname+"]=>";
		    			  }
					  }
	    			
	    			  cell=null;
	    		  }    		  
	    		 
	    		 
	    	  }else{
	    		  if(reversYes.isSelected()){
    				  rule+="<=>";
    			  }else{
    				  rule+="=>";
    			  }
	    	  }
	    	  
	    	  
	    	  if(reversYes.isSelected()){	    		 
	    		  edgeData.setReversible(true);
	    		  if(doubleRevers.isSelected()){
	    			  
	    			  reversRuleProducts+=rule;  			  
	    			  edgeData.setReversibilityType(ReversibilityType.DOUBLE);
	    			  if(reversModulator.isSelected()){
	    				  reacts=modulatorSelected2.getText();
	    		    	  if(!reacts.contains("Selected")){
	    		    		  DefaultGraphCell cell=GraphUtilities.getCellByName(graph,reacts);
	    		    		  BiochamEntityData dt=(BiochamEntityData)cell.getUserObject();
	        				  if(dt.getRepresentingName()==null){
	        					  dt.setRepresentingName(reacts);
	        				  }
	    		    		  if(cell instanceof ENucleicAcidFeatureCell){
	    		    			  reversRuleModulator+="=[#"+dt.getRepresentingName()+"]=>";
	    					  }else{
	    						  reversRuleModulator+="=["+dt.getRepresentingName()+"]=>";
	    					  }	    		    		
	    		    		  edgeData.setReversibleModulator(cell);
	    		    		  cell=null;
	    		    	  }else{
	    		    		  reversRuleModulator+="=>";
	    		    	  }
	    			  }
	    		  }else{
	    			  edgeData.setReversibilityType(ReversibilityType.SINGLE);
	    		  }
	    	  }
	    	  
	    	  if(reactants.size()>0){
	    		  
		    	  if(this.dialogName.contains("State")){
			    	  elems.clear();
			    	  reacts=productsSelected.getText().trim();
			    	  st=new StringTokenizer(reacts," ");
			    	  while(st.hasMoreTokens()){
			    		  elems.add(st.nextToken());
			    	  }
			    	  
			    	  
			    	  for(int i=0;i<elems.size();i++){
			    		  
			    		  if(elems.get(i).contains("(")){ //(Cdc2,3)
			    	
			    			  String name=elems.get(i).substring(1,elems.get(i).indexOf(";"));
			    			  int stoich=0;
			    			  try{
			    				  stoich=Integer.valueOf(elems.get(i).substring(elems.get(i).indexOf(";")+1,elems.get(i).length()-1));
			    			  }catch(Exception e){}
			    			  if(name.equals("Source/Sink")){
			    				  BiochamEntityData data=new BiochamEntityData(graph);
			    				  data.setName("Source/Sink");			    				  
			    				  ESourceSink sourceSink=new ESourceSink(data);
			    			
			    				  graph.getGraphLayoutCache().insert(sourceSink);
			    				  products.add(new BiochamObject(sourceSink.addPort(),stoich,"Source/Sink",sourceSink));
			    				  reversRuleReactants="_";
			    				  rule+="_";
			    			  }else{
			    				  DefaultGraphCell cell=GraphUtilities.getCellByName(graph,name);
			    				  BiochamEntityData dt=(BiochamEntityData)cell.getUserObject();
		        				  
			    				  Object port=cell.addPort();
			    				  products.add(new BiochamObject(port,stoich,name,cell));
			    				  if(cell instanceof ENucleicAcidFeatureCell){
			    					  if(stoich>1){
				    					  rule+=stoich+"*#"+name;  	
				    					  reversRuleReactants+=stoich+"*#"+name;
				    				  }else{
				    					  rule+="#"+name;
				    					  reversRuleReactants+="#"+name;
				    				  }
				    				  if(i<elems.size()-1){
				    					  rule+="+";
				    					  reversRuleReactants+="+";				    					  
				    				  }
		    					  }else{
		    						  if(stoich>1){
				    					  rule+=stoich+"*"+name;  	
				    					  reversRuleReactants+=stoich+"*"+name;
				    				  }else{
				    					  rule+=name;
				    					  reversRuleReactants+=name;
				    				  }
				    				  if(i<elems.size()-1){
				    					  rule+="+";
				    					  reversRuleReactants+="+";				    					  
				    				  }
		    					  }	
			    				  cell=null;
			    			  }
			    			  
			    			  
			    		  }else{ //Cdc2
			    		
			    			  if(elems.get(i).equals("Source/Sink")){
			    				  BiochamEntityData data=new BiochamEntityData(graph);
			    				  data.setName("Source/Sink");
			    				  ESourceSink sourceSink=new ESourceSink(data);			    				
			    				  graph.getGraphLayoutCache().insert(sourceSink);
			    				  products.add(new BiochamObject(sourceSink.addPort(),1,"Source/Sink",sourceSink));	
			    				  rule+="_";
			    				  reversRuleReactants="_";
			    				  data=null;
			    				  sourceSink=null;
			    			  }else{
			    				  DefaultGraphCell cell=GraphUtilities.getCellByName(graph,elems.get(i));
			    				  if(cell!=null){		
			    					  BiochamEntityData dt=(BiochamEntityData)cell.getUserObject();
			        				  if(dt.getRepresentingName()==null){
			        					  dt.setRepresentingName(elems.get(i));
			        				  }
			    					  products.add(new BiochamObject(cell.addPort(),1,dt.getRepresentingName(),cell));	
			    					  if(i<elems.size()-1){
				    					  rule+="+";
				    					  reversRuleReactants+="+";
				    				  }
			    				  }
			    				  cell=null;
			    			  }
			    		  }
			    	  }
		    	  }else{
		    		  
		    		  if(reactants.size()>0){
			    		  ArrayList<ContainingMolecule> cMols=null;
			    		  if(dialogName.contains("Association")){
			    			  
			    			  cMols=new ArrayList<ContainingMolecule>();
				    		  String name=reactants.get(0).getParentName();
				    		  name=((BiochamEntityData)reactants.get(0).getInstance().getUserObject()).getName();
				    		  int stoich=reactants.get(0).getStoichiometry();
				    		
				    		  if(reactants.size()>1){
				    			  
				    			  String nm="",nm2="";
					    		  
				    			  for(int i=0;i<reactants.size();i++){
					    			
				    				  DefaultGraphCell cell=reactants.get(i).getInstance();
					    			  
				    				  if(cell!=null){
					    				
				    					  name=reactants.get(i).getParentName();
					    				  BiochamEntityData data=(BiochamEntityData)cell.getUserObject();
					    				  name=data.getName();
					    				  String type=GraphUtilities.getCellType(cell);
					    				  int k1=reactants.get(i).getStoichiometry();
					    				  int k2=data.getMultimerCardinality();
					    				  String represName="";
					    				  if(k2>1 && name.contains("-")){
					    					  String tmpStr=name.substring(name.lastIndexOf("-")+1,name.length());	
					    					  represName=name.substring(0,name.indexOf(tmpStr))+tmpStr;
					    				  }else{
					    					  represName=name;
					    				  }
					    				  if(k1<1){
					    					  k1=1;
					    				  }
					    				  if(k2<1){
					    					  k2=1;
					    				  }	
					    				  String namm=name;
				    					  name="";
				    					  stoich=k1*k2;
				    					  if(k2>1 && k1==1){
				    						  name=namm;
				    					  }else{
				    					  for(int j=0;j<stoich;j++){
				    						  name+=namm;
				    						  if(j<stoich-1){
				    							  name+="-";
				    						  }
				    					  }
				    					  }				    					  
				    					  data.setRepresentingName(represName);
				    					  data.setMoleculeType(type);
					    				  cMols.add(new ContainingMolecule(name,data.getMoleculeState(),k1*k2,data.isModulator(),type,data.getInitialConcentration(),represName,data.getId(),cell));
					    				  
					    				  if(cell instanceof ENucleicAcidFeatureCell){
					    					  if(i>0){
						    					  nm2+="-#"+name;
						    					  nm+="-"+name;
						    				  }else{
						    					  nm2="#"+name;
						    					  nm=name;
						    				  }
					    				  }else{
					    					  if(i>0){
						    					  nm+="-"+name;
						    				  }else{
						    					  nm=name;
						    				  }
					    				  }
					    				 
					    			  }	    			  
					    		  }
					    		  BiochamEntityData data=new BiochamEntityData(graph);
					    		  data.setContainingMolecules(cMols);
					    		  data.setName(nm);					    
					    		  EComplexCell cc=new EComplexCell(data);
					    		  products.add(new BiochamObject(cc.addPort(),1,nm,cc));
					    		  graph.getGraphLayoutCache().insert(cc);
					    		  if(nm2!=""){
					    			  rule+=nm2;
					    			  reversRuleReactants+=nm2;  
					    		  }else{
					    			  rule+=nm;
					    			  reversRuleReactants+=nm;  
					    		  }
					    		  
						    	
					    		
					    		  
					    		  
				    		  }else if(reactants.size()==1){
				    			  DefaultGraphCell cell=GraphUtilities.getCellByName(graph,name);		    			 
				    			  if(reactants.get(0).getStoichiometry()>1){
				    				  if(GraphUtilities.getCellType(cell).contains("Complex")){
				    					  BiochamEntityData data=new BiochamEntityData(graph);
				    					  data.setMultimerCardinality(reactants.get(0).getStoichiometry());
				    					  data.setMoleculeState(((BiochamEntityData)cell.getUserObject()).getMoleculeState());
				    					  String nm=name;
				    					  for(int i=1;i<reactants.get(0).getStoichiometry();i++){
				    						  nm+="-"+name;
				    					  }
				    					  data.setName(nm);
				    					  data.setContainingMolecules(((BiochamEntityData)reactants.get(0).getInstance().getUserObject()).getContainingMolecules());
				    					  EComplexCell cc=new EComplexCell(data);
							    		  products.add(new BiochamObject(cc.addPort(),data.getMultimerCardinality(),nm,cc));
							    		  rule+=nm;
							    		  reversRuleReactants+=nm;
							    		  graph.getGraphLayoutCache().insert(cc);
					    			  }else if(GraphUtilities.getCellType(cell).contains("Nucleic")){
					    				  BiochamEntityData data=new BiochamEntityData(graph);
				    					  data.setMultimerCardinality(reactants.get(0).getStoichiometry());
				    					  data.setMoleculeState(((BiochamEntityData)cell.getUserObject()).getMoleculeState());
				    					  String nm=name, nm2="#"+name;
				    					  for(int i=1;i<reactants.get(0).getStoichiometry();i++){
				    						  nm+="-"+name;
				    						  nm2+="-#"+name;
				    					  }
				    					 
				    					//  String nm2=GraphUtilities.getNucleicName(nm);
				    					  data.setName(nm);
				    					  ENucleicAcidFeatureCell cc=new ENucleicAcidFeatureCell(data);
							    		  products.add(new BiochamObject(cc.addPort(),data.getMultimerCardinality(),nm,cc));
							    		  rule+=nm2;
							    		  reversRuleReactants+=nm2;
							    		  graph.getGraphLayoutCache().insert(cc);
					    			  }else{
					    				  BiochamEntityData data=new BiochamEntityData(graph);
				    					  data.setMultimerCardinality(reactants.get(0).getStoichiometry());
				    					  data.setMoleculeState(((BiochamEntityData)cell.getUserObject()).getMoleculeState());
				    					  String nm=name;
				    					  for(int i=1;i<reactants.get(0).getStoichiometry();i++){
				    						  nm+="-"+name;
				    					  }
				    					  data.setName(nm);
				    					  DefaultGraphCell cc=null;
				    					  nm="#"+nm;
				    					  if(nm.startsWith("#")){
												cc=new ENucleicAcidFeatureCell(data);
												data.setMoleculeType("NucleicAcidFeature");
											}else{
												cc=new EMacromoleculeCell(data);
												data.setMoleculeType("Macromolecule");
											}		
							    		  products.add(new BiochamObject(cc.addPort(),data.getMultimerCardinality(),nm,cc));
							    		  rule+=nm;
							    		  reversRuleReactants+=nm;
							    		  graph.getGraphLayoutCache().insert(cc);
					    			  }
				    			  }
				    			 
				    		  }
				    		 
			    		  }else if(dialogName.contains("Dissociation")){
			    			  
			    			  String name=reactants.get(0).getParentName();
			    			  DefaultGraphCell cell=GraphUtilities.getCellByName(graph,name);
			    			  BiochamEntityData data;
			    			  if(cell!=null){
			    				  data=(BiochamEntityData)cell.getUserObject();
			    				  
			    				  if(cell instanceof EComplexCell && data.getContainingMolecules()!=null){
				    				  data=(BiochamEntityData)cell.getUserObject();
				    				  cMols=data.getContainingMolecules();
				    				  if(cMols!=null){
				    					  
				    					  for(int i=0;i<cMols.size();i++){
				    						  
				    						  ContainingMolecule c=cMols.get(i);
				    						  int card=c.getCardinality();
				    						  String nm="",nm2="";
				    						  if(card>1){
				    							  String tmpStr=c.getName().substring(c.getName().lastIndexOf("-")+1,c.getName().length());	
				    							  nm=c.getName().substring(0,c.getName().indexOf(tmpStr))+tmpStr;
				    							  if(c.instance instanceof ENucleicAcidFeatureCell){
				    								  nm2="#"+nm;  
				    							  }
				    							  
										
				    						  }else{
				    							  nm=c.getName();
				    							  if(c.instance instanceof ENucleicAcidFeatureCell){
				    								  nm2="#"+nm;  
				    							  }
				    						  }
				    						  DefaultGraphCell cc=GraphUtilities.getCellByName(graph,nm);
				    						  int k1=data.getMultimerCardinality();
											  int k2=reactants.get(0).getStoichiometry();
											
											  if(k1<1){
												  k1=1;
											  }
											  if(k2<1){
												  k2=1;
											  }
											  if(card<1){
												  card=1;
											  }
											  products.add(new BiochamObject(cc.addPort(),k1*k2*card,nm,cc));
											  if(k1*k2*card>1){
												  if(cc instanceof ENucleicAcidFeatureCell){
													  rule+=k1*k2*card+"*"+nm2;
													  reversRuleReactants+=k1*k2*card+"*"+nm2; 
				    							  }else{
				    								  rule+=k1*k2*card+"*"+nm;
				    								  reversRuleReactants+=k1*k2*card+"*"+nm;
				    							  }
											  }else{
												  if(cc instanceof ENucleicAcidFeatureCell){
													  rule+="#"+c.getName();
													  reversRuleReactants+="#"+c.getName();
				    							  }else{
				    								  rule+=c.getName();
													  reversRuleReactants+=c.getName();  
				    							  }
												  
											  }
											  if(i<cMols.size()-1){
						    					  rule+="+";
						    					  reversRuleReactants+="+";
						    				  }
				    					  }
				    				  }
				    			  }else if(data.getContainingMolecules()==null && data.getMultimerCardinality()>1){
				    				  
				    				  String nm;
				    					
				    				  String tmpStr=data.getName().substring(data.getName().lastIndexOf("-")+1,data.getName().length());	
				    				  nm=data.getName().substring(0,data.getName().indexOf(tmpStr))+tmpStr;
				    				  DefaultGraphCell c=GraphUtilities.getCellByName(graph,nm);
				    				  if(c!=null){
				    					  products.add(new BiochamObject(c.addPort(),1,nm,c));		    					
				    				  }else{
				    					  BiochamEntityData d=new BiochamEntityData(graph);
				    					  d.setMoleculeState(data.getMoleculeState());
				    					  d.setName(nm);
				    					  String type=GraphUtilities.getCellType(cell);
				    					  DefaultGraphCell cc;
				    					  if(type.contains("Nucleic")){				    						 
				    						  cc=new ENucleicAcidFeatureCell(d);
				    					  }else if(type.equals("Complex")){
				    						  cc=new EComplexCell(d);
				    					  }else{
				    						  cc=new EMacromoleculeCell(d);
				    					  }
				    					  graph.getGraphLayoutCache().insert(cc);
				    					  products.add(new BiochamObject(cc.addPort(),data.getMultimerCardinality(),nm,cc));
				    				  }
				    				  if(c instanceof ENucleicAcidFeatureCell){
				    					  rule+=data.getMultimerCardinality()+"*#"+nm;
					    				  reversRuleReactants+=data.getMultimerCardinality()+"*#"+nm;
	    							  }else{
	    								  rule+=data.getMultimerCardinality()+"*"+nm;
					    				  reversRuleReactants+=data.getMultimerCardinality()+"*"+nm;  
	    							  }
				    				  
				    				  
				    			  }else{
				    				  JOptionPane.showMessageDialog(this,"\nThe reactant of Dissociation has to be a Complex or a Multimer. Otherwise, use StateTransition to draw your reaction.\n");
				    				  for(int i=0;i<addedItems.size();i++){
				    		    	
				    		    		  removeItemFromComboLists(addedItems.get(i));	    		  
				    		    		  graph.setSelectionCell(GraphUtilities.getCellByName(graph,addedItems.get(i)));
				    		    			if (!graph.isSelectionEmpty()) {
				    		    				Object[] cells = graph.getSelectionCells();
				    		    				cells = graph.getDescendants(cells);
				    		    				graph.getModel().remove(cells);
				    		    			}
				    		    	  }   	  
				    				  setVisible(false);
				    			      dispose();
				    			      rule="";
				    			      reversRuleReactants="";
				    			  }
			    				  
			    			  }
			    			  
			    		  }
			    	  }else{

			    		  String name=reactants.get(0).getParentName();
		    			  DefaultGraphCell cell=GraphUtilities.getCellByName(graph,name);		    			 
		    			  if(reactants.get(0).getStoichiometry()>1){
		    				  if(GraphUtilities.getCellType(cell).equals("Complex")){
		    					  BiochamEntityData data=new BiochamEntityData(graph);
		    					  data.setMultimerCardinality(reactants.get(0).getStoichiometry());
		    					  data.setMoleculeState(((BiochamEntityData)cell.getUserObject()).getMoleculeState());
		    					  String nm=name;
		    					  for(int i=1;i<reactants.get(0).getStoichiometry();i++){
		    						  nm+="-"+name;
		    					  }
		    					  data.setName(nm);
		    					  EComplexCell cc=new EComplexCell(data);
					    		  products.add(new BiochamObject(cc.addPort(),data.getMultimerCardinality(),nm,cc));
					    		  rule+=nm;
					    		  reversRuleReactants+=nm;
					    		  graph.getGraphLayoutCache().insert(cc);
			    			  }else if(GraphUtilities.getCellType(cell).contains("Nucleic")){
			    				  BiochamEntityData data=new BiochamEntityData(graph);
		    					  data.setMultimerCardinality(reactants.get(0).getStoichiometry());
		    					  data.setMoleculeState(((BiochamEntityData)cell.getUserObject()).getMoleculeState());
		    					  String nm=name;
		    					  for(int i=1;i<reactants.get(0).getStoichiometry();i++){
		    						  nm+="-"+name;
		    					  }
		    					  String nm2=GraphUtilities.getNucleicName(nm);
		    					  data.setName(nm2);
		    					  data.setName(nm2);
		    					  ENucleicAcidFeatureCell cc=new ENucleicAcidFeatureCell(data);
					    		  products.add(new BiochamObject(cc.addPort(),data.getMultimerCardinality(),nm2,cc));
					    		  rule+="#"+nm2;
					    		  reversRuleReactants+="#"+nm2;
					    		  graph.getGraphLayoutCache().insert(cc);
			    			  }else{
			    				  BiochamEntityData data=new BiochamEntityData(graph);
		    					  data.setMultimerCardinality(reactants.get(0).getStoichiometry());
		    					  data.setMoleculeState(((BiochamEntityData)cell.getUserObject()).getMoleculeState());
		    					  String nm=name;
		    					  for(int i=1;i<reactants.get(0).getStoichiometry();i++){
		    						  nm+="-"+name;
		    					  }
		    					  data.setName(nm);
		    					  DefaultGraphCell cc=null;
		    					  if(nm.startsWith("#")){
										cc=new ENucleicAcidFeatureCell(data);
										data.setMoleculeType("NucleicAcidFeature");
									}else{
										cc=new EMacromoleculeCell(data);
										data.setMoleculeType("Macromolecule");
									}		
					    		  products.add(new BiochamObject(cc.addPort(),data.getMultimerCardinality(),nm,cc));
					    		  rule+=nm;
					    		  reversRuleReactants+=nm;
					    		  graph.getGraphLayoutCache().insert(cc);
			    			  }
		    			  }
			    	  }
		    	  }
	    	  }	      
	    	  if(reactants.size()>0){
	    		  if(modulators1!=null){
	    			  if(modulators1.size()>0){
	    				  for(int i=0;i<modulators1.size();i++){
	    					  rule+="+"+modulators1.get(i);
	    				  }
	    			  }
	    		  }
		    	  for(int i=0;i<products.size();i++){
						molecules.add(((BiochamEntityData)products.get(i).getInstance().getUserObject()).getId());
		    	  }
		    	  for(int i=0;i<reactants.size();i++){
		    		  molecules.add(((BiochamEntityData)reactants.get(i).getInstance().getUserObject()).getId());
		    	  }
		    	
		    	  if(edgeData.getModulator1()!=null){
		    		  molecules.add(((BiochamEntityData)edgeData.getModulator1().getUserObject()).getId());
		    	  }
		    	  if(edgeData.getReversibleModulator()!=null){
		    		  molecules.add(((BiochamEntityData)edgeData.getReversibleModulator().getUserObject()).getId());
		    	  }
		    	  edgeData.setMolecules(molecules);
		    	  
		    	  
		    	  
		    	  
		    	  
		    	  
		    	  // CREATING THE REACTIONS ON THE GRAPH................		    	  
		    	  if(this.dialogName.contains("State")){
		    		  if(reversRuleReactants!="" && reversRuleProducts!=""){
		    			  String reversRule=reversRuleReactants+reversRuleModulator+reversRuleProducts;
		    		
		    			  if(rule.contains(" ")){
		    				  rule=rule.replaceAll(" ","");
		    			  }
		    			  

		    				edgeData=GraphUtilities.setReactants(edgeData, reactants);
		    				edgeData=GraphUtilities.setProducts(edgeData, products);
		    				if(edgeData.getModulator1()!=null){
		    					edgeData.getModulators().add(new BiochamEnitity(((BiochamEntityData)edgeData.getModulator1().getUserObject()).getId(),((BiochamEntityData)edgeData.getModulator1().getUserObject()).getName()));
			    			  }
			    			  if(edgeData.getReversibleModulator()!=null){
			    				  edgeData.getModulators().add(new BiochamEnitity(((BiochamEntityData)edgeData.getReversibleModulator().getUserObject()).getId(),((BiochamEntityData)edgeData.getReversibleModulator().getUserObject()).getName()));
			    			  }
		    				if(modulators1!=null){
		    					for(int i=0;i<modulators1.size();i++){
		    						DefaultGraphCell cl=GraphUtilities.getCellByName(graph,modulators1.get(i));
		    						if(cl!=null){
		    							edgeData.getModulators().add(new BiochamEnitity(((BiochamEntityData)cl.getUserObject()).getId(),((BiochamEntityData)cl.getUserObject()).getName()));
		    						}
		    					}	
		    				}
		    			  
		    			  for(int k=0;k<reactants.size();k++){
		    					((BiochamEntityData)reactants.get(k).getInstance().getUserObject()).setReactant(true);
		    			  }
		    			  for(int k=0;k<products.size();k++){
		    					((BiochamEntityData)products.get(k).getInstance().getUserObject()).setProduct(true);
		    			  }
		    			  
		    			  StateTransition st1=new StateTransition(edgeData,reactants,products,edgeData.getModulator1(),edgeData.getReversibleModulator(),graph,false,rule);
		    			  graph.getGraphLayoutCache().insert(st1);//+";"+reversRule));
		    			  if(modulators1!=null){
			    			  if(modulators1.size()>0){
			    				  for(int i=0;i<modulators1.size();i++){
			    					 st1.addModulator(modulators1.get(i),1,false);
			    				  }
			    			  }
			    		  }
		    		  }else{
		    			  edgeData=GraphUtilities.setReactants(edgeData, reactants);
		    				edgeData=GraphUtilities.setProducts(edgeData, products);
		    				if(edgeData.getModulator1()!=null){
		    					edgeData.getModulators().add(new BiochamEnitity(((BiochamEntityData)edgeData.getModulator1().getUserObject()).getId(),((BiochamEntityData)edgeData.getModulator1().getUserObject()).getName()));
			    			  }
			    			  if(edgeData.getReversibleModulator()!=null){
			    				  edgeData.getModulators().add(new BiochamEnitity(((BiochamEntityData)edgeData.getReversibleModulator().getUserObject()).getId(),((BiochamEntityData)edgeData.getReversibleModulator().getUserObject()).getName()));
			    			  }
		    				if(modulators1!=null){
		    					for(int i=0;i<modulators1.size();i++){
		    						DefaultGraphCell cl=GraphUtilities.getCellByName(graph,modulators1.get(i));
		    						if(cl!=null){
		    							edgeData.getModulators().add(new BiochamEnitity(((BiochamEntityData)cl.getUserObject()).getId(),((BiochamEntityData)cl.getUserObject()).getName()));
		    						}
		    					}	
		    				}
		    			  
		    			  for(int k=0;k<reactants.size();k++){
		    					((BiochamEntityData)reactants.get(k).getInstance().getUserObject()).setReactant(true);
		    				}
		    				for(int k=0;k<products.size();k++){
		    					((BiochamEntityData)products.get(k).getInstance().getUserObject()).setProduct(true);
		    				}		
		    				
		    			  StateTransition st1=new StateTransition(edgeData,reactants,products,edgeData.getModulator1(),edgeData.getReversibleModulator(),graph,false,rule);
		    			  graph.getGraphLayoutCache().insert(st1);		    			  
		    			  if(modulators1!=null){
			    			  if(modulators1.size()>0){
			    				  for(int i=0;i<modulators1.size();i++){
			    					 st1.addModulator(modulators1.get(i),1,false);
			    				  }
			    			  }
			    		  }
		    		  }
		    		      		  
		  
		    			  
		    	  }else if(this.dialogName.contains("Association")){
		    		  
		    		  edgeData=GraphUtilities.setReactants(edgeData, reactants);
	    				edgeData=GraphUtilities.setProducts(edgeData, products);
	    				if(edgeData.getModulator1()!=null){
	    					edgeData.getModulators().add(new BiochamEnitity(((BiochamEntityData)edgeData.getModulator1().getUserObject()).getId(),((BiochamEntityData)edgeData.getModulator1().getUserObject()).getName()));
		    			  }
		    			  if(edgeData.getReversibleModulator()!=null){
		    				  edgeData.getModulators().add(new BiochamEnitity(((BiochamEntityData)edgeData.getReversibleModulator().getUserObject()).getId(),((BiochamEntityData)edgeData.getReversibleModulator().getUserObject()).getName()));
		    			  }
	    				if(modulators1!=null){
	    					for(int i=0;i<modulators1.size();i++){
	    						DefaultGraphCell cl=GraphUtilities.getCellByName(graph,modulators1.get(i));
	    						if(cl!=null){
	    							edgeData.getModulators().add(new BiochamEnitity(((BiochamEntityData)cl.getUserObject()).getId(),((BiochamEntityData)cl.getUserObject()).getName()));
	    						}
	    					}	
	    				}
	    			  edgeData.setStoichiometry(products.get(0).getStoichiometry());
	    			  for(int i=0;i<reactants.size();i++){
	    					((BiochamEntityData)reactants.get(i).getInstance().getUserObject()).setReactant(true);
	    				}
	    			  ((BiochamEntityData)products.get(0).getInstance().getUserObject()).setProduct(true);
	    			  if(edgeData.getModulator1()!=null){
	    				  ((BiochamEntityData)edgeData.getModulator1().getUserObject()).setModulator(true);
	    			  }
		    		  Association st1=new Association(edgeData,reactants,products.get(0),edgeData.getModulator1(),graph,rule);
		    		  graph.getGraphLayoutCache().insert(st1);		    			  
	    			  if(modulators1!=null){
		    			  if(modulators1.size()>0){
		    				  for(int i=0;i<modulators1.size();i++){
		    					 st1.addModulator(modulators1.get(i),1,false);
		    				  }
		    			  }
		    		  }
		    		  
		    	  }else if(this.dialogName.contains("Dissociation")){
		    		  
		    		  edgeData=GraphUtilities.setReactants(edgeData, reactants);
	    				edgeData=GraphUtilities.setProducts(edgeData, products);
	    				if(edgeData.getModulator1()!=null){
	    					edgeData.getModulators().add(new BiochamEnitity(((BiochamEntityData)edgeData.getModulator1().getUserObject()).getId(),((BiochamEntityData)edgeData.getModulator1().getUserObject()).getName()));
		    			  }
		    			  if(edgeData.getReversibleModulator()!=null){
		    				  edgeData.getModulators().add(new BiochamEnitity(((BiochamEntityData)edgeData.getReversibleModulator().getUserObject()).getId(),((BiochamEntityData)edgeData.getReversibleModulator().getUserObject()).getName()));
		    			  }
	    				if(modulators1!=null){
	    					for(int i=0;i<modulators1.size();i++){
	    						DefaultGraphCell cl=GraphUtilities.getCellByName(graph,modulators1.get(i));
	    						if(cl!=null){
	    							edgeData.getModulators().add(new BiochamEnitity(((BiochamEntityData)cl.getUserObject()).getId(),((BiochamEntityData)cl.getUserObject()).getName()));
	    						}
	    					}	
	    				}
		  			edgeData.setStoichiometry(reactants.get(0).getStoichiometry());
		  			  
		  			for(int i=0;i<products.size();i++){
		  				((BiochamEntityData)products.get(i).getInstance().getUserObject()).setProduct(true);
		  			}
		  			((BiochamEntityData)reactants.get(0).getInstance().getUserObject()).setReactant(true);
		  			if(edgeData.getModulator1()!=null){
		  				((BiochamEntityData)edgeData.getModulator1().getUserObject()).setModulator(true);
		  			}
					
		  			Dissociation st1=new Dissociation(edgeData,reactants.get(0),products,edgeData.getModulator1(),graph,rule);
		  			graph.getGraphLayoutCache().insert(st1);		    			  
		  			if(modulators1!=null){
		  				if(modulators1.size()>0){
		  					for(int i=0;i<modulators1.size();i++){
		  						st1.addModulator(modulators1.get(i),1,false);
		  					}
		  				}
		  			}
		    	  }
		    	  GraphUtilities.addReactionFromGraphToModel(graph,rule);		  		 
	    	  }*/
	    	 
	    	
	    		 //removeTemporalGraphObjects();   
	    	//}	    	  
	    	closeWindow = true;
	  		
	      }else if(cmd.equals("CANCEL")){
	    	  
	    	  //remove the all added molecules from the combolists.......
	    	  for(int i=0;i<addedItems.size();i++){
	    	
	    		  removeItemFromComboLists(addedItems.get(i));	    		  
	    		  graph.setSelectionCell(GraphUtilities.getCellByName(graph,addedItems.get(i)));
	    			if (!graph.isSelectionEmpty()) {
	    				Object[] cells = graph.getSelectionCells();
	    				cells = graph.getDescendants(cells);
	    				graph.getModel().remove(cells);
	    			}
	    	  }   	  
	    	  
	    	  closeWindow = true;
	      }
	      
	      
	      	      
	      if (closeWindow) {
	         setVisible(false);
	         dispose();
	      }
	      
	   }



	/**
	 * 
	 */
	public static void removeTemporalGraphObjects() {
		if(addedItems!=null){
			for(int i=0;i<addedItems.size();i++){
			    	
				  //removeItemFromComboLists(addedItems.get(i));	    		  
				  graph.setSelectionCell(GraphUtilities.getCellByName(graph,addedItems.get(i)));
					if (!graph.isSelectionEmpty()) {
						Object[] cells = graph.getSelectionCells();
						cells = graph.getDescendants(cells);
						graph.getModel().remove(cells);
					}
			}
		}
	}

	
	
	class ComboboxkeyListener extends KeyAdapter{
		
		public void keyPressed(KeyEvent e) {

			if(e.getSource() instanceof CustomMultiSelectComboBox){
				
				CustomMultiSelectComboBox cb=(CustomMultiSelectComboBox)e.getSource();				
				if(e.getKeyCode()==KeyEvent.VK_ENTER){				
									
					
					cb.hidePopup();
					setTextFieldsEditable(true);
					cb.getParentClass().setFinished(true);    				
    				ArrayList selected=cb.getParentClass().getSelected();
    				ArrayList all = new ArrayList(cb.getParentClass().getAllItems());
    				boolean allSelected=false;
    				
    				for(int i=0;i<selected.size();i++){
    					
    					
    					if(selected.contains("Select All")){
    						
    						selectedIndividuallyR="";
    						selectedIndividuallyP="";
    						selectedIndividuallyM1="";
    						selectedIndividuallyM2="";
    						if(cb.getItemCount()<cb.getParentClass().getAllItems().size()){
    							cb.removeAllItems();
    							cb.addItem("Select All");
    							for(int k=0;k<cb.getParentClass().getAllItems().size();k++){
    								cb.addItem(cb.getParentClass().getAllItems().get(k));
    							}
    						}
    						boolean noDefCoeff=false;
    						int coeff=1;
    						if(!cb.getName().contains("modulator")){    							
        						String coefStoich= (String)JOptionPane.showInputDialog(instance,"Enter coefficient of stoichiometry for the selected "+cb.getName()+":\n","Coefficient of Stoichiometry"); 
        						if(coefStoich!=null){
        							try{
        								coeff=Integer.valueOf(coefStoich);
        							}catch(Exception ex){
        								JOptionPane.showMessageDialog(instance,"Error writing the coefficient of stoichiometry.","Warning",JOptionPane.WARNING_MESSAGE);
        								coeff=1;
        							}
        						}        					
        						if(coeff>1){
        							noDefCoeff=true;
        						}
        						
    						} 
    						
    						allSelected=true;
    						all.remove("Source/Sink");
    						if(allSelected){        					
    	    					selected=all;
    	    				}
    						
    						int ll=cb.getParentClass().getAllItems().size();
    						String selectedItems="";
    	    				for(int j=0;j<selected.size();j++){
    	    					if(noDefCoeff && !cb.getName().contains("modulator")){
    	    						selectedItems+="("+selected.get(j)+";"+coeff+") ";
    	    					}else{
    	    						selectedItems+=selected.get(j)+" ";
    	    					}
    			       		} 
    	    				if(cb.getName().equals("reactants")){
    	    					reactantsSelected.setText("");
    	    					reactantsSelected.setText(selectedItems);
    	    				}else if(cb.getName().equals("products")){
    	    					productsSelected.setText("");
    	    					productsSelected.setText(selectedItems);
    	    				}else if(cb.getName().equals("modulator1")){
    	    					modulatorSelected1.setText("");
    	    					modulatorSelected1.setText(selectedItems);
    	    				}else if(cb.getName().equals("modulator2")){
    	    					modulatorSelected2.setText("");
    	    					modulatorSelected2.setText(selectedItems);
    	    				}
    	    				cb.setPopupVisible(false);
    	    				cb.getBTip().setVisible(false);
    	    				cb.getBTip().setEnabled(false);
    	    				setTextFieldsEditable(false);
    	    				cb.getParentClass().desellectAll(); 
    	    				cb.getParentClass().getSelected().clear();
    	    				cb.getParentClass().setFinished(false);
    						break;
    					
    					
    					}else if(selected.get(i).equals("Source/Sink")){
    						selectedIndividuallyR="";
    						selectedIndividuallyP="";
    						selectedIndividuallyM1="";
    						selectedIndividuallyM2="";
    						if(cb.getItemCount()<cb.getParentClass().getAllItems().size()){
    							cb.removeAllItems();
    							cb.addItem("Select All");
    							for(int k=0;k<cb.getParentClass().getAllItems().size();k++){
    								cb.addItem(cb.getParentClass().getAllItems().get(k));
    							}
    						}    						
    						if(cb.getName().equals("reactants")){
    	    					reactantsSelected.setText("");
    	    					reactantsSelected.setText(selected.get(i).toString());
    	    				}else if(cb.getName().equals("products")){
    	    					productsSelected.setText("");
    	    					productsSelected.setText(selected.get(i).toString());
    	    				}
    	    				cb.setPopupVisible(false);
    	    				cb.getBTip().setVisible(false);
    	    				cb.getBTip().setEnabled(false);
    	    				setTextFieldsEditable(false);
    	    				cb.getParentClass().desellectAll();    	    				
    	    	
    	    				cb.getParentClass().getSelected().clear();
    	    				cb.getParentClass().setFinished(false);
    	    		
    	    				break;
    				
    					
    					}else{
    						
    						String n=cb.getName();    						
    						if(!cb.getName().contains("modulator")){    	
	    						int coeff=1;
	    						String coefStoich= (String)JOptionPane.showInputDialog(instance,"Enter coefficient of stoichiometry for the selected "+cb.getName()+":\n","Coefficient of Stoichiometry"); 
	    						if(coefStoich!=null){
	    							try{
	    								coeff=Integer.valueOf(coefStoich);
	    							}catch(Exception ex){
	    								JOptionPane.showMessageDialog(instance,"Error writing the coefficient of stoichiometry.","Warning",JOptionPane.WARNING_MESSAGE);
	    								coeff=1;
	    							}
	    						} 
	    					
	    	    			
	    	    				for(int j=0;j<selected.size();j++){
	    	    					
		    						if(n.startsWith("r")){
		    						
		    	    						selectedIndividuallyR+="("+selected.get(j)+";"+coeff+") ";
		    	    				
		    						}else if(n.startsWith("p")){
		    						
		    	    						selectedIndividuallyP+="("+selected.get(j)+";"+coeff+") ";
		    	    				
		    						}  else if(n.equals("modulator1")){
		    						
	    	    						selectedIndividuallyM1+="("+selected.get(j)+";"+coeff+") ";
	    	    				
		    						} else if(n.equals("modulator2")){
		    						
	    	    						selectedIndividuallyM2+="("+selected.get(j)+";"+coeff+") ";
	    	    				
		    						}    	   	    					
	    			       		} 
    						}else{
    							
	    	    				for(int j=0;j<selected.size();j++){
	    	    					
		    						if(n.startsWith("r")){
		    						
		    	    						selectedIndividuallyR+=selected.get(j)+" ";
		    	    				
		    						}else if(n.startsWith("p")){
		    						
		    	    						selectedIndividuallyP+=selected.get(j)+" ";
		    	    				
		    						}else if(n.equals("modulator1")){
		    						
	    	    						selectedIndividuallyM1+=selected.get(j)+" ";
	    	    				
		    						}else if(n.equals("modulator2")){
		    						
	    	    						selectedIndividuallyM2+=selected.get(j)+" ";
	    	    				
		    						}      	   	    					
	    			       		} 
    						}
    	    				
    	    				if(n.equals("reactants")){
    	    					reactantsSelected.setText("");
    	    					reactantsSelected.setText(selectedIndividuallyR);
    	    				}else if(n.equals("products")){
    	    					productsSelected.setText("");
    	    					productsSelected.setText(selectedIndividuallyP);
    	    				}    	else if(cb.getName().equals("modulator1")){
    	    					modulatorSelected1.setText("");
    	    					modulatorSelected1.setText(selectedIndividuallyM1);
    	    				}else if(cb.getName().equals("modulator2")){
    	    					modulatorSelected2.setText("");
    	    					modulatorSelected2.setText(selectedIndividuallyM2);
    	    				}
    	    			
            		        CustomIconedCellRenderer rdr=cb.getParentClass().getRenderer();    		      
            		        Vector selectedObjectsVector=rdr.getSelectedObjects();
            		        for(int j=0;j<selectedObjectsVector.size();j++){
            		        	Object o=selectedObjectsVector.get(j);
            		        	cb.removeItem(o);
            		        	
            		        }        		        
    	    				cb.setPopupVisible(false);
    	    				cb.getBTip().setVisible(false);
    	    				cb.getBTip().setEnabled(false);
    	    				setTextFieldsEditable(false);
    	    				cb.getParentClass().desellectAll();    	    				
    	    		
    	    				cb.getParentClass().getSelected().clear();
    	    				cb.getParentClass().setFinished(false);
    					}
    				}    
				} 
				cb.getBTip().setVisible(false);
				cb.getBTip().setEnabled(false);				
			}
		}
	}
	
	class ComboBoxPopupListener implements PopupMenuListener{
		
		
		public void popupMenuCanceled(PopupMenuEvent e) {
			if(e.getSource() instanceof CustomMultiSelectComboBox){
				CustomMultiSelectComboBox cb=(CustomMultiSelectComboBox)e.getSource();
				int k=cb.getParentClass().getSelected().size();
				
				if(k>0){
					cb.getBTip().setVisible(true);
				}else{
					cb.getBTip().setVisible(false);
				}
			}else if(e.getSource() instanceof SimpleComboBox){
				SimpleComboBox cb=(SimpleComboBox)e.getSource();
			
				int ind=cb.getSelectedIndex();
				if(ind>0){
					cb.getBTip().setVisible(true);
				}else{
					cb.getBTip().setVisible(false);
				}
			}			
		}

		public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
			
			
		}
		

		public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
			if(e.getSource() instanceof CustomMultiSelectComboBox){
				CustomMultiSelectComboBox cb=(CustomMultiSelectComboBox)e.getSource();
			
				cb.getBTip().setVisible(false);	
				
				
				
					Dimension d = cb.getPreferredSize();
					int maxWidth=d.width;
					for(int i=0;i<cb.getItemCount();i++){
						if(cb.getItemAt(i).toString().length()>maxWidth){
							maxWidth=cb.getItemAt(i).toString().length();
						}
					}
					((CustomMultiSelectComboBox)cb).setPreferredSize(new Dimension(130, d.height));
					((CustomMultiSelectComboBox)cb).setPopupWidth(maxWidth+40);    
				
			}else if(e.getSource() instanceof SimpleComboBox){
				SimpleComboBox cb=(SimpleComboBox)e.getSource();
				
				cb.getBTip().setVisible(true);	
			}			
		}
		
	}

	public void setListWithoutSourceSink(String[] listWithoutSourceSink) {
		this.listWithoutSourceSink = listWithoutSourceSink;
	}

	public void setListWithSourceSink(String[] listWithSourceSink) {
		this.listWithSourceSink = listWithSourceSink;
	}
	
	
	
	
}
