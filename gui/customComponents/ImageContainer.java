package fr.inria.contraintes.biocham.customComponents;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.menus.CustomPopupDotImage;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.imageio.ImageIO;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;



/**
 * Class that creates a custom JPanel component 
 * 
 * @author Dragana Jovanovska  
 */ 
public class ImageContainer extends JPanel implements ActionListener{

	
	CustomZoomableImagePanel image;
	JPopupMenu dotImagePopup;
	
	
	
	public ImageContainer(CustomZoomableImagePanel child){
		
		super();
		createUI(child);
       
        
	}

	
	
	public ImageContainer(CustomZoomableImagePanel child,String name){
		
		super();
		setName(name);
		createUI(child);
        
	}

	
	/**
	 * @param child
	 */
	private void createUI(CustomZoomableImagePanel child) {
		dotImagePopup=new CustomPopupDotImage(this).getPopup();	
		addMouseListener(new MouseAdapter(){
			  public void mousePressed(MouseEvent ev) {
			        if (ev.isPopupTrigger()) {
			        	dotImagePopup.show(ev.getComponent(), ev.getX(), ev.getY());
			        }
			      }
			      public void mouseReleased(MouseEvent ev) {
			        if (ev.isPopupTrigger()) {
			        	dotImagePopup.show(ev.getComponent(), ev.getX(), ev.getY());
			        }
			      }
		  });
		this.setLayout(new FlowLayout(FlowLayout.CENTER));
		child.setM_imageContainer(this);
        setBackground(Color.WHITE);	                 
        add(child);	
        image=child;
	}
	
	public void actionPerformed(ActionEvent e) {
		

		
		if(e.getActionCommand().equals("editRGraph")){
			
			BiochamDynamicTree.workbench.replaceTabbedPane(BiochamDynamicTree.currentModel).setVisible(true);
		}else if(e.getActionCommand().equals("printDotImage")){
			
		}else if(e.getActionCommand().equals("saveDotImage")){
			
			SwingWorker sw=new SwingWorker(){

				//File f1;
				Object[] possibilities;
				String extension;
				
				@Override
				public Object construct() {
					String[] imageFormats=ImageIO.getWriterFormatNames();
					
					

					for(int i=0;i<imageFormats.length;i++){
						imageFormats[i]= imageFormats[i].toLowerCase();
					}
					java.util.Arrays.sort(imageFormats);
					int counter=0;
					for(int i=0,j=i+1;i<imageFormats.length-1;i++,j++){
						if(!imageFormats[i].equals(imageFormats[j])){
							counter++;
						}
					}				   
					possibilities = new String[counter];
					for(int i=0,j=i+1,k=0;i<imageFormats.length-1;i++,j++){
						if(!imageFormats[i].equals(imageFormats[j])){
							possibilities[k]=imageFormats[i];
							k++;
						}
					}
					return possibilities;
				}
				
				@Override
				public void finished(){
					String s = (String)JOptionPane.showInputDialog(BiochamMainFrame.frame,
							" \nChoose Save Format:\n",
							"Save Image As",
							JOptionPane.PLAIN_MESSAGE,
							Icons.icons.get("File-1-48x48.png"+1.5),
							(Object[]) getValue(),
							"png");
					if(s!=null){
						extension=s;
					}else{
						extension="png";
					}
					final String rep=Utils.showSaveDialog("",BiochamMainFrame.frame,"image");			
					if (rep!=null) {
						SwingWorker ss=new SwingWorker(){

							@Override
							public Object construct() {
								image.saveToImage(rep,extension);
								return null;
							}

							@Override
							public void finished() {
								// TODO Auto-generated method stub
								
							}};
						ss.start();
						 
			    	}
				}
				
			};
			
			sw.start();
			
		}else if(e.getActionCommand().equals("zoomIn")){
			
			image.zoomIn();       
			image.adjustLayout();
			  
		}else if(e.getActionCommand().equals("zoomOut")){
			
			image.zoomOut();
			image.adjustLayout();
			  
		}
		
	
		
	}

}
