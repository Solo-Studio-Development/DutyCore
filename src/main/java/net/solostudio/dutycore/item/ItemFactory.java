package net.solostudio.dutycore.item;

import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.processor.MessageProcessor;
import org.bukkit.Material;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.ItemFlag;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public interface ItemFactory {
    static ItemFactory create(@NotNull Material material) {
        return new ItemBuilder(material);
    }

    static ItemFactory create(@NotNull Material material, int count) {
        return new ItemBuilder(material, count);
    }

    static ItemFactory create(@NotNull Material material, int count, short damage) {
        return new ItemBuilder(material, count, damage);
    }

    static ItemFactory create(@NotNull Material material, int count, short damage, byte data) {
        return new ItemBuilder(material, count, damage, data);
    }

    static ItemFactory create(ItemStack item) {
        return new ItemBuilder(item);
    }

    ItemFactory setType(@NotNull Material material);

    ItemFactory setCount(int newCount);

    int getSlot();


    ItemFactory setSlot(int slot);

    ItemFactory setName(@NotNull String name);

    void addEnchantment(@NotNull Enchantment enchantment, int level);

    default ItemFactory addEnchantments(Map<Enchantment, Integer> enchantments) {
        enchantments.forEach(this::addEnchantment);

        return this;
    }

    ItemBuilder addLore(@NotNull String... lores);

    ItemFactory setUnbreakable();

    default void addFlag(@NotNull ItemFlag... flags) {
        Arrays
                .stream(flags)
                .forEach(this::addFlag);
    }

    default ItemFactory setLore(@NotNull String... lores) {
        Arrays
                .stream(lores)
                .forEach(this::addLore);
        return this;
    }

    ItemFactory removeLore(int line);

    ItemStack finish();

    boolean isFinished();

    private static Optional<ItemStack> buildItem(@NotNull ConfigurationSection section) {
        return Optional.ofNullable(section.getString("material"))
                .map(Material::valueOf)
                .map(material -> {
                    int amount = section.getInt("amount", 1);
                    String name = section.getString("name", "");

                    return ItemFactory.create(material, amount)
                            .setName(name)
                            .addLore(section.getStringList("lore")
                                    .stream()
                                    .map(MessageProcessor::process).toArray(String[]::new))
                            .finish();
                });
    }

    static Optional<ItemStack> createItemFromString(@NotNull String path) {
        return Optional.ofNullable(DutyCore.getInstance().getConfiguration().getSection(path))
                .flatMap(ItemFactory::buildItem);
    }

    static void createItemFromString(@NotNull String path, @NotNull Inventory inventory) {
        Optional.ofNullable(DutyCore.getInstance().getConfiguration().getSection(path))
                .flatMap(section -> {
                    var itemOpt = buildItem(section);
                    int slot = section.getInt("slot", 0);
                    itemOpt.ifPresent(item -> inventory.setItem(slot, item));
                    return itemOpt;
                });
    }

    static int getItemSlotFromString(@NotNull String path) {
        return Optional.ofNullable(DutyCore.getInstance().getConfiguration().getSection(path))
                .map(section -> section.getInt("slot", -1))
                .orElse(-1);
    }
}
