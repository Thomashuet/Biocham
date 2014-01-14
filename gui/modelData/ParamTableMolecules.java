package fr.inria.contraintes.biocham.modelData;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.BiochamModelElement;
import fr.inria.contraintes.biocham.WorkbenchArea;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.dialogs.OutputDialog;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.Spring;
import javax.swing.SpringLayout;

public class ParamTableMolecules implements Parameters,ActionListener{

	
	public Vector<Molecule> molecules;
	WorkbenchArea biocham;
	JPanel panel;
	BiochamModel model;
	BiochamModelElement element;
	boolean checked=false;
	ArrayList<String> checkMolecules;
	int numMolecules=0;
	int savedResponses;
	MoleculesModel moleculesModel;
	MoleculesView view;
	
	public MoleculesView getView() {
		return view;
	}
	public void setView(MoleculesView view) {
		this.view = view;
	}
	public MoleculesModel getMoleculesModel() {
		return moleculesModel;
	}
	public void setMoleculesModel(MoleculesModel moleculesModel) {
		this.moleculesModel = moleculesModel;
	}
	public int getPanelCurrentX(){
		int x=0;
		Spring maxSpring = Spring.constant(10);
		SpringLayout layout = (SpringLayout) panel.getLayout();
		int i=1;
		/*if(panel.getComponentCount()>3){
			i=3;
		}*/
		maxSpring=layout.getConstraint(SpringLayout.WEST, panel.getComponent(panel.getComponentCount()-i));
		x=maxSpring.getValue()+80;
		return x;
	}
	public int getPanelCurrentY(){
		int y=0;
		Spring maxSpring = Spring.constant(10);
		SpringLayout layout = (SpringLayout) panel.getLayout();
		maxSpring=layout.getConstraint(SpringLayout.NORTH, panel.getComponent(panel.getComponentCount()-1));
		y=maxSpring.getValue()+80;
		return y;
	}
	
	
	public int getNumMolecules() {
		return numMolecules;
	}

	public void setNumMolecules(int numMolecules) {
		this.numMolecules = numMolecules;
	}

	public ParamTableMolecules(BiochamModel m,WorkbenchArea workbench, JPanel p){
		
		biocham=workbench;
		model=m;
		panel=p;
		molecules=new Vector<Molecule>();
		checkMolecules=new ArrayList<String>();
		savedResponses=-1;
		moleculesModel=new MoleculesModel(model);
		view=new MoleculesView(BiochamMainFrame.frame,moleculesModel);
	}
	
	
public class Molecule {
		
		
		private String moleculeName;

		Molecule(String m) {
	    	moleculeName=m;
	    }

		public String getMoleculeName() {
			return moleculeName;
		}

		public void setMoleculeName(String moleculeName) {
			this.moleculeName = moleculeName;
		}
		
		public String toString(){
			return moleculeName;
		}
			
}



	public String getName(int i) {
		return molecules.get(i).getMoleculeName();
	}

	public String getValue(int i) {
		return getName(i);
	}

	public int indexOf(String paramName) {
		
		int i=0;
    	while(i<molecules.size() && !getName(i).equals(paramName))
    		 i++;    	
        if (i == molecules.size())
           return -1;
        return i;
        
	}

	public void resetParameter(String s) {
		// Molecule can't be modified
		
	}

	public void setValue(ArrayList list) {
		
		String molecule=(String)list.get(0);
		int i=indexOf(molecule);
		if(i<0){
			
			Component[] comps=panel.getComponents();
	        numMolecules++;
			for(int k=0;k<comps.length;k++){
				
			
				if(comps[k].getName().equals("checkMolecules")){
	        		panel.remove(k);	       		 	
	        	}
			}			
			molecules.add(new Molecule(molecule)); //vector of parameters
			moleculesModel.addMolecule(molecule);
			
			int numParam = molecules.size()-1;
			SpringLayout layout = (SpringLayout) panel.getLayout();
			JTextField l = new JTextField(molecule);
			l.setEditable(false);
			l.setBackground(Utils.backgroundColor);
			l.setName(molecule);  
			panel.add(l);	    	
	    	layout.putConstraint(SpringLayout.WEST, l, LEFT_OFF,SpringLayout.WEST, panel);
	    	layout.putConstraint(SpringLayout.NORTH, l, TOP_OFF+HEIGHT*numParam,SpringLayout.NORTH, panel);
	        	
		}
		
	}

	public int size() {
		return molecules.size();
	}

	public void setModified(Component comp) {
		// Molecule can't be modified
		
	}

	synchronized public JPanel refreshPanel(ArrayList cs) {
		
		SpringLayout layout = (SpringLayout) panel.getLayout();
		Spring maxSpring = Spring.constant(MIDDLE);
		int i;
		
		for(i=0;i<cs.size();i++){
			
			JComponent cp=(JComponent) cs.get(i);				
			if(cp instanceof JTextField){				
				
				JTextField l=(JTextField)cp;
				panel.add(l);
				layout.putConstraint(SpringLayout.WEST, l, LEFT_OFF,SpringLayout.WEST, panel);
			    layout.putConstraint(SpringLayout.NORTH, l, TOP_OFF+HEIGHT*i,SpringLayout.NORTH, panel);
			}
		}
		       
        String toolTipText="<html><i>Checks the lower/upper case errors in molecules' names,<br> and checks if there are production and " +
		"degradation <br> rules for them.</i></html>";
       
        CustomToolTipButton b1=new CustomToolTipButton("Check",toolTipText);
        b1.setActionCommand("checkMolecules");
        b1.setName("checkMolecules");
        b1.addActionListener(this);        
				
        Component[] comps=panel.getComponents();
        int compNum=comps.length;
        if(compNum>0 && !(comps[0] instanceof JButton)){
			
        	 panel.add(b1);
			 for (i=0;i<comps.length;++i)
		        	maxSpring = Spring.max(maxSpring, Spring.sum(layout.getConstraints(panel.getComponent(i)).getWidth(),Spring.constant(10)));
			 
			 layout.putConstraint(SpringLayout.NORTH, b1, TOP_OFF+HEIGHT*(i+1),SpringLayout.NORTH, panel);			        
		     layout.putConstraint(SpringLayout.WEST, b1, maxSpring, SpringLayout.WEST, panel);
			 
		}	
	    cs.clear();
	    panel.validate();
		panel.repaint();		
        
        
        
		return panel;
	}

	public boolean isChecked() {
		return checked;
	}

	public void setChecked(boolean checked) {
		this.checked = checked;
	}
	
	
	public void actionPerformed(ActionEvent e) {
		
		if(e.getSource() instanceof CustomToolTipButton){
			CustomToolTipButton b=(CustomToolTipButton)e.getSource();
			b.setBalloonToolTipVisible(false);
		}
		String command = e.getActionCommand();        
	    if (command.equals("checkMolecules")) {
	    	model.checkMolecules();
	    	checkMolecules(BiochamMainFrame.frame);
	    }
		
	}

	public void checkMolecules(JFrame parent) {
		final OutputDialog od=new OutputDialog("Checked molecules", parent);
		 JButton okButton = new JButton();
		 okButton.setText("Ok");
		 okButton.setActionCommand("Ok");
		 okButton.addActionListener(new ActionListener() {public void actionPerformed(ActionEvent event) {  od.setVisible(false);
		 od.dispose();}});
		if(checkMolecules.size()>0){
			od.getCoverPanel().setLayout(new GridLayout(checkMolecules.size()+1,1));
			for(int i=0;i<checkMolecules.size();i++){
				od.getCoverPanel().add(new JLabel(checkMolecules.get(i)));
			}
			
			//od.getCoverPanel().add(okButton);
			od.setSize(400,checkMolecules.size()*50);
			od.showVisible();
			/*SpringLayout layout = (SpringLayout) panel.getLayout();
			for(int i=0;i<checkMolecules.size();i++){
				JTextField l=new JTextField(checkMolecules.get(i));
				l.setEditable(false);
				l.setBackground(Utils.backgroundColor);
				l.setName("warning");
				panel.add(l);
				layout.putConstraint(SpringLayout.WEST,l, LEFT_OFF, SpringLayout.WEST, panel);
				int k=70+20*i;
				Spring con=Spring.sum(layout.getConstraint(SpringLayout.NORTH, panel.getComponent(numMolecules)),Spring.constant(k));
				layout.putConstraint(SpringLayout.NORTH,l, con, SpringLayout.NORTH, panel);
				BiochamDynamicTree.getCorrespondigTabbedPane().add( new UniversalScrollPane(panel));	    			
			}
			panel.validate();
			panel.repaint();*/
			
		}else{
			//JOptionPane.showMessageDialog(BiochamMainFrame.frame, "There are no errors or warnings.");
			od.getCoverPanel().setLayout(new FlowLayout());
			od.getCoverPanel().add(new JLabel("There are no errors or warnings."));//,BorderLayout.CENTER);
			//od.getCoverPanel().add(okButton);
			od.setSize(300,200);
			od.showVisible();
		}
	}

	public ArrayList<String> getCheckMolecules() {
		return checkMolecules;
	}

	public void setCheckMolecules(ArrayList<String> checkMolecules) {
		this.checkMolecules = checkMolecules;
	}
	public void clearCheckMolecules(ArrayList<String> checkMolecules) {
		this.checkMolecules.clear();
	}

	public int getSavedResponses() {
		return savedResponses;
	}

	public void setSavedResponses(int savedResponses) {
		this.savedResponses = savedResponses;
	}

	public void resetSavedResponses() {
		savedResponses=-1;
	}

	public void disposeElements() {
	
		molecules.clear();
		molecules=null;
		biocham=null;
		panel=null;
		model=null;
		element=null;
		checkMolecules.clear();
		checkMolecules=null;
		numMolecules=0;
		savedResponses=0;
		
	}

	public void addParameter() {
		// TODO Auto-generated method stub		
	}

}
