package net.solostudio.dutycore.menu.menus;

import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.data.StaffData;
import net.solostudio.dutycore.enums.keys.ConfigKeys;
import net.solostudio.dutycore.data.MenuController;
import net.solostudio.dutycore.enums.keys.ItemKeys;
import net.solostudio.dutycore.enums.keys.MessageKeys;
import net.solostudio.dutycore.menu.PaginatedMenu;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.event.inventory.InventoryCloseEvent;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.SkullMeta;
import org.jetbrains.annotations.NotNull;
import java.util.ArrayList;
import java.util.List;

@SuppressWarnings("all")
public class StaffMenu extends PaginatedMenu {
    public StaffMenu(@NotNull MenuController controller) {
        super(controller);
    }

    @Override
    public String getMenuName() {
        return ConfigKeys.STAFF_MENU_TITLE.getString();
    }

    @Override
    public int getSlots() {
        return ConfigKeys.STAFF_MENU_SIZE.getInt();
    }

    @Override
    public int getMenuTick() {
        return ConfigKeys.STAFF_MENU_TICK.getInt();
    }

    @Override
    public void setMenuItems() {
        inventory.clear();
        addMenuBorder();

        DutyCore.getDatabase().getStaffDatas().thenAccept(staffs -> {
            int startIndex = page * getMaxItemsPerPage();
            int endIndex = Math.min(startIndex + getMaxItemsPerPage(), staffs.size());

            staffs.subList(startIndex, endIndex)
                    .stream()
                    .map(this::createStaffItem)
                    .forEach(inventory::addItem);
        });
    }

    @Override
    public void handleMenu(final InventoryClickEvent event) {
        if (!(event.getWhoClicked() instanceof Player player) || !event.getInventory().equals(inventory)) return;

        event.setCancelled(true);

        int slot = event.getSlot();
        DutyCore.getDatabase().getStaffDatas().thenAccept(staffs -> {
            if (slot == ItemKeys.FORWARD_ITEM.getSlot()) handlePageChange(player, staffs.size(), true);
            else if (slot == ItemKeys.BACK_ITEM.getSlot()) handlePageChange(player, staffs.size(), false);
            else if (slot == ItemKeys.MAIN_ITEM.getSlot()) Bukkit.getScheduler().runTask(DutyCore.getInstance(), () -> new NavigationMenu(MenuController.getMenuUtils(player)).open());
        });
    }

    @EventHandler
    public void onClose(final InventoryCloseEvent event) {
        if (event.getInventory().equals(inventory)) close();
    }

    private void handlePageChange(@NotNull Player player, int totalItems, boolean isForward) {
        int totalPages = (int) Math.ceil((double) totalItems / getMaxItemsPerPage());
        int newPage = page + (isForward ? 1 : -1);

        if (newPage < 0 || newPage >= totalPages) {
            player.sendMessage(isForward ? MessageKeys.LAST_PAGE.getMessage() : MessageKeys.FIRST_PAGE.getMessage());
            return;
        }

        page = newPage;
        super.open();
    }

    private ItemStack createStaffItem(@NotNull StaffData staff) {
        ItemStack itemStack = ItemKeys.STAFF_ITEM.getItem();
        SkullMeta meta = (SkullMeta) itemStack.getItemMeta();

        if (itemStack.getType() == Material.PLAYER_HEAD) {
            meta.setOwner(staff.name());
            meta.setDisplayName(meta.getDisplayName().replace("{player}", staff.name()));

            List<String> lore = meta.getLore() != null ? new ArrayList<>(meta.getLore()) : new ArrayList<>();
            lore = replacePlaceholders(lore, staff);
            meta.setLore(lore);

            itemStack.setItemMeta(meta);
        }

        return itemStack;
    }

    private List<String> replacePlaceholders(List<String> lore, @NotNull StaffData staff) {
        List<String> newLore = new ArrayList<>();

        lore.forEach(line -> {
            line = line.replace("{inDuty}", staff.isInDuty() ? ConfigKeys.TRUE.getString() : ConfigKeys.FALSE.getString())
                    .replace("{sRank}", staff.staffRank())
                    .replace("{pRank}", staff.playerRank())
                    .replace("{dTime}", staff.dutyTime())
                    .replace("{bans}", String.valueOf(staff.bans()))
                    .replace("{kicks}", String.valueOf(staff.kicks()))
                    .replace("{mutes}", String.valueOf(staff.mutes()))
                    .replace("{badge}", staff.badge());
            newLore.add(line);
        });

        return newLore;
    }



}
