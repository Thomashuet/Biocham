package fr.inria.contraintes.biocham.customComponents;

import fr.inria.contraintes.biocham.utils.Utils;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceMotionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.TransferHandler;




/**
 * Class thats creates a custom UniversalScrollPane that holds a panel that is draggable. 
 * Used in PlotsComparizonWindowUtils.java 
 * 
 * @author Dragana Jovanovska  
 */ 
public class DraggablePanel extends UniversalScrollPane implements Transferable{

	
	
	public DraggablePanel(JPanel p){
		super(p);
		setBackground(Utils.backgroundColor);
		//setLayout(new BorderLayout());
		//add(new JLabel(ic),BorderLayout.CENTER);
		 // Add the listener which will export this panel for dragging
        this.addMouseListener(new MyDraggableMouseListener());        
        // Add the handler, which negotiates between drop target and this 
        // draggable panel
        this.setTransferHandler(new MyDragAndDropTransferHandler());
	}

	public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException, IOException {
		// System.out.println("Step 7 of 7: Returning the data from the Transferable object. In this case, the actual panel is now transfered!");
	        
	        DataFlavor thisFlavor = null;

	        try {
	            thisFlavor = DroppableMultiSplitPane.getDragAndDropPanelDataFlavor();
	        } catch (Exception ex) {
	            System.err.println("Problem lazy loading: " + ex.getMessage());
	            ex.printStackTrace(System.err);
	            return null;
	        }

	        // For now, assume wants this class... see loadDnD
	        if (thisFlavor != null && flavor.equals(thisFlavor)) {
	            return DraggablePanel.this;
	        }

	        return null;
	}

	public DataFlavor[] getTransferDataFlavors() {
		
		DataFlavor[] flavors = {null};
	        
		//System.out.println("Step 4 of 7: Querying for acceptable DataFlavors to determine what is available. Our example only supports our custom RandomDragAndDropPanel DataFlavor.");
	        
		try {
			flavors[0] = DroppableMultiSplitPane.getDragAndDropPanelDataFlavor();
		} catch (Exception ex) {
			System.err.println("Problem lazy loading: " + ex.getMessage());
			ex.printStackTrace(System.err);
			return null;
		}

		return flavors;
	}

	public boolean isDataFlavorSupported(DataFlavor flavor) {
		 
		//System.out.println("Step 6 of 7: Verifying that DataFlavor is supported.  Our example only supports our custom RandomDragAndDropPanel DataFlavor.");
	        
		DataFlavor[] flavors = {null};
		try {
			flavors[0] = DroppableMultiSplitPane.getDragAndDropPanelDataFlavor();
		} catch (Exception ex) {
			System.err.println("Problem lazy loading: " + ex.getMessage());
			ex.printStackTrace(System.err);
			return false;
		}

		for (DataFlavor f : flavors) {
			if (f.equals(flavor)) {
				return true;
			}
		}

		return false;
	}	
}

/**
 * <p>Listener that make source draggable.</p>
 * <p>Thanks, source modified from: http://www.zetcode.com/tutorials/javaswingtutorial/draganddrop/</p>
 */
class MyDraggableMouseListener extends MouseAdapter {

    @Override()
    public void mousePressed(MouseEvent e) {
       // System.out.println("Step 1 of 7: Mouse pressed. Going to export our RandomDragAndDropPanel so that it is draggable.");
        
        JComponent c = (JComponent) e.getSource();
        TransferHandler handler = c.getTransferHandler();
        handler.exportAsDrag(c, e, TransferHandler.MOVE);
    }
} // DraggableMouseListener

/**
 * <p>Used by both the draggable class and the target for negotiating data.</p>
 * <p>Note that this should be set for both the draggable object and the drop target.</p>
 * @author besmit
 */
class MyDragAndDropTransferHandler extends TransferHandler implements DragSourceMotionListener {

    public MyDragAndDropTransferHandler() {
        super();
    }

    /**
     * <p>This creates the Transferable object. In our case, RandomDragAndDropPanel implements Transferable, so this requires only a type cast.</p>
     * @param c
     * @return
     */
    @Override()
    public Transferable createTransferable(JComponent c) {

       // System.out.println("Step 3 of 7: Casting the RandomDragAndDropPanel as Transferable. The Transferable RandomDragAndDropPanel will be queried for acceptable DataFlavors as it enters drop targets, as well as eventually present the target with the Object it transfers.");
        
        // TaskInstancePanel implements Transferable
        if (c instanceof DraggablePanel) {
            Transferable tip = (DraggablePanel) c;
            return tip;
        }

        // Not found
        return null;
    }

    public void dragMouseMoved(DragSourceDragEvent dsde) {}

    /**
     * <p>This is queried to see whether the component can be copied, moved, both or neither. We are only concerned with copying.</p>
     * @param c
     * @return
     */
    @Override()
    public int getSourceActions(JComponent c) {
        
        //System.out.println("Step 2 of 7: Returning the acceptable TransferHandler action. Our RandomDragAndDropPanel accepts Copy only.");
        
        if (c instanceof DraggablePanel) {
            return TransferHandler.MOVE;
        }
        
        return TransferHandler.NONE;
    }
} // DragAndDropTransferHandler