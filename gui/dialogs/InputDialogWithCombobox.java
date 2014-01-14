package fr.inria.contraintes.biocham.dialogs;

import fr.inria.contraintes.biocham.customComponents.CustomComboBox;

import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.SpringLayout;




public class InputDialogWithCombobox extends JDialog{

	
	public static String chosenValue=null;
    CustomComboBox pbox;
	
	public InputDialogWithCombobox(JFrame parent, String message, String title, final JComboBox cbox,CustomComboBox parentBox){
		
		super(parent,title,true);
	
		pbox=parentBox;
		SpringLayout sl=new SpringLayout();
		Container contents = getContentPane();
	    contents.setLayout(sl);
		JLabel l1=new JLabel(message);
		contents.add(l1);
		contents.add(cbox);
		JButton ok=new JButton("OK");
		ok.setActionCommand("ok");
		ok.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
				
				setChosenValue(cbox.getSelectedItem().toString());
				setVisible(false);
				dispose();			
				
			}});
		contents.add(ok);
		
		JButton cancel=new JButton("CANCEL");
		cancel.setActionCommand("cancel");
		cancel.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
				pbox.desellectAll();
				setChosenValue(null);
				setVisible(false);
				dispose();			
				
			}});
		contents.add(cancel);
		
		sl.putConstraint(SpringLayout.WEST, l1, 20, SpringLayout.WEST, contents);
		sl.putConstraint(SpringLayout.NORTH, l1, 20, SpringLayout.NORTH, contents);
		sl.putConstraint(SpringLayout.WEST, cbox, 70, SpringLayout.WEST, contents);
		sl.putConstraint(SpringLayout.NORTH, cbox, 30, SpringLayout.SOUTH, l1);
		sl.putConstraint(SpringLayout.WEST, ok, 100, SpringLayout.WEST, contents);
		sl.putConstraint(SpringLayout.NORTH, ok, 30, SpringLayout.SOUTH, cbox);
		sl.putConstraint(SpringLayout.WEST, cancel, 10, SpringLayout.EAST, ok);
		sl.putConstraint(SpringLayout.NORTH, cancel, 30, SpringLayout.SOUTH, cbox);
		
		this.setLocationRelativeTo(null);
		setSize(300,200);
		setVisible(true);  
		pack();  
		
	}

	
	
	
	
	
	
	
	
	public String getChosenValue() {
		return chosenValue;
	}
	public void setChosenValue(String chosenValue) {
		this.chosenValue = chosenValue;
	}
	
	
	
}
