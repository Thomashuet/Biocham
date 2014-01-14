package fr.inria.contraintes.biocham.dialogs;


import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Component;

import javax.swing.JDialog;
import javax.swing.JOptionPane;

public class DialogInitialConcentration {

	public static JDialog getDialog(Component p,String n){
		JOptionPane pane=new JOptionPane();
		pane.createDialog(p, n);
		pane.setBackground(Utils.backgroundColor);
		pane.setVisible(true);
		return null;
	}
}
