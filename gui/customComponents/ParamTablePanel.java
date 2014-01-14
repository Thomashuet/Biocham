package fr.inria.contraintes.biocham.customComponents;

import java.awt.Color;
import fr.inria.contraintes.biocham.utils.Utils;
import javax.swing.BorderFactory;
import javax.swing.JPanel;

/**
 * Common looking panel for the model's components (rules, concentrations, parameters, declarations, etc...)
 * 
 * @author Dragana Jovanovska  
 */ 
public class ParamTablePanel extends JPanel{

	public ParamTablePanel(String elName){
		setBackground(Utils.backgroundColor);
		setName(elName);
		setBorder(BorderFactory.createTitledBorder(BorderFactory.createLineBorder(Color.black),elName));
	}
}
