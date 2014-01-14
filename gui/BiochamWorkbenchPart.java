package fr.inria.contraintes.biocham;

import fr.inria.contraintes.biocham.documentation.BiochamDocumentation;

import javax.swing.JComponent;
import javax.swing.SwingUtilities;




/**
 * Class that is responsable for creating instances of the screens.
 * 
 */
public class BiochamWorkbenchPart {
	
	
	
	static JComponent screen;
	
	/**
	 * It returns an instance of the screen which name is given as a parameter.
	 * @param name The name of the screen which instance if being demanded.
	 * 
	 */
	public static JComponent getScreen(String name) {

		
		boolean b=SwingUtilities.isEventDispatchThread();
		
		if(name.equals("Workbench")){
		
			screen=new WorkbenchArea();
		
		}
		else if(name.equals("Documentation")){
		
			screen=new BiochamDocumentation();
			screen.addKeyListener(BiochamMainFrame.instance);
			
		}
			
		return screen;
	}
		
	

}
