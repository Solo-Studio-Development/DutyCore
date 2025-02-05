package net.solostudio.dutycore.menu.menus;

import net.solostudio.dutycore.enums.keys.ConfigKeys;
import net.solostudio.dutycore.enums.keys.ItemKeys;
import net.solostudio.dutycore.data.MenuController;
import net.solostudio.dutycore.menu.Menu;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.event.inventory.InventoryCloseEvent;
import org.jetbrains.annotations.NotNull;

@SuppressWarnings("all")
public class NavigationMenu extends Menu {
    public NavigationMenu(@NotNull MenuController controller) {
        super(controller);
    }

    @Override
    public String getMenuName() {
        return ConfigKeys.NAVIGATION_MENU_TITLE.getString();
    }

    @Override
    public int getSlots() {
        return ConfigKeys.NAVIGATION_MENU_SIZE.getInt();
    }

    @Override
    public int getMenuTick() {
        return Integer.MAX_VALUE;
    }

    @Override
    public void handleMenu(final InventoryClickEvent event) {
        if (!(event.getWhoClicked() instanceof Player player) || !event.getInventory().equals(inventory)) return;

        event.setCancelled(true);

        int slot = event.getSlot();

        if (slot == ItemKeys.NAVIGATION_MENU_STAFF.getSlot()) {
            close();
            new StaffMenu(MenuController.getMenuUtils(player)).open();
        }
    }

    @Override
    public void setMenuItems() {
        ItemKeys.NAVIGATION_MENU_STAFF.getItem(inventory);
    }

    @EventHandler
    public void onClose(final InventoryCloseEvent event) {
        if (event.getInventory().equals(inventory)) close();
    }
}
