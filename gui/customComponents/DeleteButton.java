package fr.inria.contraintes.biocham.customComponents;

import fr.inria.contraintes.biocham.utils.Icons;

import javax.swing.JLabel;



/**
 * Class thats creates a custom JLabel component to act as an image button, which when clicked performs an action of deletion of a biocham model's property.
 * 
 * @author Dragana Jovanovska  
 */ 
public class DeleteButton extends JLabel{

	public DeleteButton(){		
		super(Icons.icons.get("delete3.gif"));
		setToolTipText("Delete");
	}
}
