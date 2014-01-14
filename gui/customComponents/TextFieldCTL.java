package fr.inria.contraintes.biocham.customComponents;

import fr.inria.contraintes.biocham.BiochamModelElement;
import fr.inria.contraintes.biocham.dialogs.DialogAddSpecification;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JFrame;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;




/**
 * Class that creates a custom JTextField component which when double-clicked opens a dialog that assist the user in composing the value.
 * 
 * @author Dragana Jovanovska  
 */ 
public class TextFieldCTL extends JTextField implements MouseListener{

	
	JFrame parentFrame;
	BiochamModelElement element;
	
	
	
	/*public TextFieldCTL(String text,BiochamModelElement me){
		super(text);
		element=me;
		addMouseListener(this);
		
	}
	*/
	public TextFieldCTL(JFrame parent,BiochamModelElement me){
		super();
		element=me;
		addMouseListener(this);
		parentFrame=parent;
	}

	
	
	
	
	public void mouseClicked(MouseEvent e) {
		if(e.getClickCount()==2){
			DialogAddSpecification params=new DialogAddSpecification(parentFrame, element,"CTL Operators",element.getModel().getCtlSpecifications().getParametersPanel());					
			String value=params.getFormula();
			
			boolean b=SwingUtilities.isEventDispatchThread();
			
			setText(value);	
		}		
	}
	public void mouseEntered(MouseEvent e) {		
	}
	public void mouseExited(MouseEvent e) {		
	}
	public void mousePressed(MouseEvent e) {
	}
	public void mouseReleased(MouseEvent e) {		
	}

}
