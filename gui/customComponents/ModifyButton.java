package fr.inria.contraintes.biocham.customComponents;

import fr.inria.contraintes.biocham.utils.Icons;

import javax.swing.JLabel;



/**
 * Class thats creates a custom JLabel component to act as an image button, which when clicked performs an action of modification of a biocham model's property.
 * 
 * @author Dragana Jovanovska  
 */ 
public class ModifyButton extends JLabel{

	public ModifyButton(){		
		super(Icons.icons.get("modify1.png"));
		setToolTipText("Modify");
	}
}
