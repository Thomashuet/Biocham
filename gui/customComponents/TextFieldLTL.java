package fr.inria.contraintes.biocham.customComponents;

import fr.inria.contraintes.biocham.BiochamModelElement;
import fr.inria.contraintes.biocham.dialogs.DialogAddSpecification;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JFrame;
import javax.swing.JTextField;


/**
 * Class that creates a custom JTextField component which when double-clicked opens a dialog that assist the user in composing the value.
 * 
 * @author Dragana Jovanovska  
 */ 
public class TextFieldLTL extends JTextField implements MouseListener{

	
	JFrame parentFrame;
	BiochamModelElement element;
	String cfVariables="";
	String queryVersion=null;
	
	
	
	public TextFieldLTL(JFrame parent, String v,String text,BiochamModelElement me){
		super(text);
		element=me;
		addMouseListener(this);
		queryVersion=v;
		parentFrame=parent;
	}	
	/*public TextFieldLTL(BiochamModelElement me){
		super();
		element=me;
		addMouseListener(this);
	}*/

	
	
	
	public void mouseClicked(MouseEvent e) {
		if(e.getClickCount()==2){
			DialogAddSpecification params=null;		
			if(queryVersion!=null){
				params=new DialogAddSpecification(queryVersion,parentFrame, element, "LTL Operators",element.getModel().getLtlSpecifications().getParametersPanel());
			}else{
				params=new DialogAddSpecification(parentFrame, element, "LTL Operators",element.getModel().getLtlSpecifications().getParametersPanel());
			}
			String value=params.getFormula();
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
