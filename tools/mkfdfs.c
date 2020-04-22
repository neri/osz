/*
    Make a floppy disk image
    Copyright(c) 2017 The MEG-OS Project
    License: BSDL
*/

#include <ctype.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>


typedef uint32_t    volume_size_t;

typedef struct DOS_BPB {
    uint16_t    bytes_per_sector;
    uint8_t     sectors_per_cluster;
    uint16_t    reserved_sectors_count;
    uint8_t     n_fats;
    uint16_t    root_entries_count;
    uint16_t    total_sectors;
    uint8_t     media_descriptor;
    uint16_t    sectors_per_fat;
    uint16_t    sectors_per_track;
    uint16_t    n_heads;
} __attribute__((__packed__)) DOS_BPB;

typedef struct DOS_EBPB {
    DOS_BPB     bpb;
    uint32_t    hidden_sectors_count;
    uint32_t    total_sectors32;
    uint8_t     physical_drive_number;
    uint8_t     flags;
    uint8_t     extended_boot_sign; /* 0x29 */
    uint32_t    volume_serial_number;
    char        volume_label[11];
    char        filesystem[8];
} __attribute__((__packed__)) DOS_EBPB;

#define DOS_EBPB_EXTENDED_BOOT_SIGN  0x29

typedef struct BOOT_SECTOR {
    uint8_t     jumps[3];
    char        oem_name[8];
    DOS_EBPB    ebpb;
    uint8_t     boot_code[0x1C0];
    uint8_t     boot_signature[2]; /* 0x55, 0xAA */
} __attribute__((__packed__)) BOOT_SECTOR;

typedef struct DOS_DIRENT {
    char        filename[11];
    uint8_t     attributes;
    uint8_t     nt_reserved;
    uint8_t     reserved[9];
    uint32_t    mtime;
    uint16_t    first_cluster;
    uint32_t    filesize;
} __attribute__((__packed__)) DOS_DIRENT;

#define FAT_ATTR_READONLY   0x01
#define FAT_ATTR_HIDDEN     0x02
#define FAT_ATTR_SYSTEM     0x04
#define FAT_ATTR_LABEL      0x08
#define FAT_ATTR_SUBDIR     0x10
#define FAT_ATTR_ARCHIVE    0x20

typedef struct internal_DPB {
    uint32_t    bytes_per_cluster;
    uint32_t    total_clusters;
    uint32_t    offset_fat;
    uint32_t    offset_root;
    uint32_t    offset_cluster;
    uint32_t    last_cluster_allocated;
    int         fattype;
    DOS_BPB     bpb;
} internal_DPB;


DOS_BPB* parse_opt_f(const char* spec) {
    if (!strcasecmp(spec, "2hd") || !strcmp(spec, "1440")) {
        static DOS_BPB bpb_1440 = {512, 1, 1, 2, 224, 80 * 2 * 18, 0xF0, 9, 18, 2};
        return &bpb_1440;
    } else if (!strcasecmp(spec, "2hc") || !strcmp(spec, "1200")) {
        static DOS_BPB bpb_1200 = {512, 1, 1, 2, 224, 80 * 2 * 15, 0xF9, 7, 15, 2};
        return &bpb_1200;
    } else if (!strcasecmp(spec, "nec") || !strcmp(spec, "1232")) {
        static DOS_BPB bpb_1232 = {1024, 1, 1, 2, 192, 77 * 2 * 8, 0xFE, 2, 8, 2};
        return &bpb_1232;
    } else if (!strcasecmp(spec, "2dd") || !strcmp(spec, "720")) {
        static DOS_BPB bpb_720 = {512, 2, 1, 2, 112, 80 * 2 * 9, 0xF9, 3, 9, 2};
        return &bpb_720;
    } else if (!strcmp(spec, "640")) {
        static DOS_BPB bpb_640 = {512, 2, 1, 2, 112, 80 * 2 * 8, 0xFB, 2, 8, 2};
        return &bpb_640;
    } else if (!strcmp(spec, "320")) {
        static DOS_BPB bpb_320 = {512, 2, 1, 2, 112, 40 * 2 * 8, 0xFF, 2, 8, 2};
        return &bpb_320;
    } else if (!strcmp(spec, "160")) {
        static DOS_BPB bpb_160 = {512, 1, 1, 2, 64, 40 * 1 * 8, 0xFE, 1, 8, 1};
        return &bpb_160;
    }
    return NULL;
}


uint32_t time_to_dos_file_time(time_t *t) {
    struct tm *tm = localtime(t);
    return (tm->tm_sec / 2) +
           ((tm->tm_min) << 5) +
           ((tm->tm_hour) << 11) +
           ((tm->tm_mday) << 16) +
           ((tm->tm_mon + 1) << 21) +
           ((tm->tm_year - 80) << 25);
}


uint32_t generate_volume_serial_number() {
    uint32_t result;
    time_t t;
    struct tm *tm;
    uint16_t cx0, dx0, cx1, dx1;

    time(&t);
    tm = localtime(&t);

    cx0 = tm->tm_year + 1900;
    dx0 = ((tm->tm_mon + 1) << 8) + tm->tm_mday;
    cx1 = ((tm->tm_hour) << 8) + tm->tm_min;
    dx1 = ((tm->tm_sec) << 8);
    result = (cx0 + cx1) + ((dx0 + dx1) << 16);

    return result;
}

int fat_validate_filename_char(char c){
    switch (c) {
    case 0x21:
    case 0x23 ... 0x29:
    case 0x2D:
    case '0' ... '9':
    case 'A' ... 'Z':
    case 0x5E:
    case 0x5F:
    case 0x7B:
    case 0x7D:
    case 0x7E:
        return c;
    case 'a' ... 'z':
        return c - 0x20;
    default:
        return 0;
    }
}

int fat_name_to_dirent(DOS_DIRENT* dir, const char* name) {

    int no_ext = 0, name_has_lower = 0, name_has_upper = 0, ext_has_lower = 0, ext_has_upper = 0;
    const char* p=name;
    memset(dir->filename, ' ', 11);

    int i;
    for(i = 0; i < 8; i++) {
        char c = *p++;
        name_has_upper |= isupper(c);
        name_has_lower |= islower(c);
        if(c == 0) {
            no_ext = 1;
            break;
        } else if(c == '.') {
            break;
        } else {
            char c1 = fat_validate_filename_char(c);
            if(c1) {
                dir->filename[i] = c1;
            } else {
                return 1;
            }
        }
    }
    if (i == 8 && *p == '.') p++;
    if(!no_ext) {
        for(i = 8; i < 11; i++) {
            char c = *p++;
            ext_has_upper |= isupper(c);
            ext_has_lower |= islower(c);
            if(!c) {
                break;
            } else {
                char c1 = fat_validate_filename_char(c);
                if(c1) {
                    dir->filename[i] = c1;
                } else {
                    return 1;
                }
            }
        }
    }
    dir->nt_reserved = (name_has_lower & !name_has_upper ? 0x08 : 0)
        | (ext_has_lower & !ext_has_upper ? 0x10 : 0);

    if (dir->filename[0] == ' ') {
        return 1;
    }

    return 0;
}

static const char* basename(const char* path) {
    char sep = '/';
    const char* p = strrchr(path, sep);
    if (p) {
        return p+1;
    } else {
        return path;
    }
}


void init_dpb(internal_DPB* result, const DOS_BPB* bpb) {
    internal_DPB dpb;
    uint32_t bytes_per_sector = bpb->bytes_per_sector;
    uint32_t cluster2;

    dpb.bpb = *bpb;
    dpb.bytes_per_cluster = bytes_per_sector * dpb.bpb.sectors_per_cluster;
    dpb.offset_fat = bytes_per_sector * dpb.bpb.reserved_sectors_count;
    dpb.offset_root = dpb.offset_fat + bytes_per_sector * (dpb.bpb.n_fats * dpb.bpb.sectors_per_fat);

    cluster2 = (dpb.offset_root + dpb.bpb.root_entries_count * 32) / bytes_per_sector;
    dpb.total_clusters = (dpb.bpb.total_sectors - cluster2) / dpb.bpb.sectors_per_cluster;
    dpb.offset_cluster = cluster2 * bytes_per_sector;

    if(dpb.total_clusters < 4085) {
        dpb.fattype = 12;
    } else if(dpb.total_clusters < 65525) {
        dpb.fattype = 16;
    } else {
        dpb.fattype = 32;
    }

    dpb.last_cluster_allocated = 2;

    *result = dpb;
}


uint8_t* create_disk_image(internal_DPB* dpb, const DOS_BPB* bpb, const uint8_t* ipl, const DOS_DIRENT* volume_label) {

    init_dpb(dpb, bpb);

    volume_size_t volume_size = bpb->total_sectors * bpb->bytes_per_sector;

    uint8_t* p = malloc(volume_size);
    if(!p) return NULL;
    struct BOOT_SECTOR* bs = (struct BOOT_SECTOR*)(void*)p;
    memset(p, 0, volume_size);

    // Initialize IPL
    if(ipl) {
        memcpy(p, ipl, bpb->bytes_per_sector);
    }else{
        p[0] = 0xEB;
        p[1] = 0xFE;
        p[2] = 0x90;
        bs->ebpb.extended_boot_sign = DOS_EBPB_EXTENDED_BOOT_SIGN;
        memcpy(&bs->ebpb.volume_label, "NO NAME    ", 11);
        memcpy(&bs->ebpb.filesystem, "FAT12   ", 8);
        bs->boot_signature[0] = 0x55;
        bs->boot_signature[1] = 0xAA;
    }

    //  Fix BPB
    bs->ebpb.bpb = *bpb;
    if(bs->ebpb.extended_boot_sign == DOS_EBPB_EXTENDED_BOOT_SIGN) {
        bs->ebpb.volume_serial_number = generate_volume_serial_number();
    }
    
    //  Initialize FAT
    for(int i = 0; i < bs->ebpb.bpb.n_fats; i++) {
        uint8_t* q = p + (bs->ebpb.bpb.reserved_sectors_count + i * bs->ebpb.bpb.sectors_per_fat) * bs->ebpb.bpb.bytes_per_sector;
        q[0] = bs->ebpb.bpb.media_descriptor;
        q[1] = 0xFF;
        q[2] = 0xFF;
    }

    if(volume_label) {
        if(bs->ebpb.extended_boot_sign == DOS_EBPB_EXTENDED_BOOT_SIGN) {
            memcpy(bs->ebpb.volume_label, volume_label->filename, 11);
        }

        DOS_DIRENT* dir = (DOS_DIRENT*)(p + dpb->offset_root);
        dir[0] = *volume_label;
    }

    return p;
}


void fat_write_entry(uint8_t* _fat, int fattype, int offset, int value) {
    switch(fattype){
    case 12:
        {
            int offset_fat=offset * 3 / 2;
            if(offset & 1){
                _fat[offset_fat]=((value & 0xF) <<4 ) | (_fat[offset_fat] & 0x0F);
                _fat[offset_fat + 1] = value >> 4;
            }else{
                _fat[offset_fat]=value;
                _fat[offset_fat + 1] = ((value >> 8) & 0x0F)|(_fat[offset_fat + 1] & 0xF0);
            }
            break;
        }
    case 16:
        {
            uint16_t* fat = (uint16_t*)_fat;
            fat[offset] = value;
            break;
        }
    case 32:
        {
            uint32_t* fat = (uint32_t*)_fat;
            fat[offset] = value & 0x0FFFFFFF;
            break;
        }
    }
}

static uint8_t* disk_buffer = 0;

int fat_write_fd(uint8_t* image, internal_DPB* dpb, DOS_DIRENT* dirent, int fd) {
    off_t filesize;
    uint32_t bytes_per_cluster = dpb->bytes_per_cluster;
    uint8_t* fat = image + dpb->offset_fat;
    uint8_t* cluster_pool = image + dpb->offset_cluster - 2 * bytes_per_cluster;
    if(!disk_buffer) {
        disk_buffer = malloc(bytes_per_cluster);
    }
    if((filesize = lseek(fd, 0, SEEK_END)) > 0) {
        dirent->filesize = filesize;
        uint32_t cluster = dirent->first_cluster = dpb->last_cluster_allocated;
        lseek(fd, 0, SEEK_SET);
        for(int i = 0; i < filesize; i += bytes_per_cluster, cluster++) {
            if(cluster > dpb->total_clusters) {
                return 1;
            }
            int r=read(fd, disk_buffer, bytes_per_cluster);
            if(r <= 0) {
                return 1;
            }
            memcpy(cluster_pool + cluster * bytes_per_cluster, disk_buffer, r);
            if(i + r < filesize) {
                fat_write_entry(fat, dpb->fattype, cluster, cluster + 1);
            } else {
                fat_write_entry(fat, dpb->fattype, cluster, -1);
            }
        }
        dpb->last_cluster_allocated = cluster;
    }
    return 0;
}


int file_exists(DOS_DIRENT* dir, int limit, DOS_DIRENT* dirent) {
    for(int i = 0; i < limit; i++) {
        if(memcmp(&dir[i], dirent, 11) == 0) {
            return 1;
        }
    }
    return 0;
}


int main(int argc, char** argv) {

    uint8_t* bootsector = NULL;
    DOS_BPB* current_bpb = parse_opt_f("1440");
    internal_DPB dpb;
    const char* path_image = NULL;
    const char* path_bootsector = NULL;
    const char* volume_label = NULL;
    int arg_left = 0;
    int arg_files = 0;
    int opt_touchall = 0;
    time_t time_now;
    uint32_t ctime;

    time(&time_now);
    ctime = time_to_dos_file_time(&time_now);

    //  Parse ARGV
    for(int i = 1; i < argc; i++) {
        arg_left = argc - i;
        if(argv[i][0] == '-') {
            if(arg_left > 1 && !strcmp(argv[i], "-bs")) {
                path_bootsector = argv[++i];
            } else if(arg_left > 1 && !strcmp(argv[i], "-f")) {
                const char* param = argv[++i];
                current_bpb = parse_opt_f(param);
                if(!current_bpb) {
                    fprintf(stderr, "Error: invalid format: %s\n", param);
                    return 1;
                }
            } else if(!strcmp(argv[i], "-touch")) {
                opt_touchall = 1;
            } else if(arg_left > 1 && !strcmp(argv[i], "-l")) {
                volume_label = argv[++i];
            } else {
                fprintf(stderr, "Error: invalid option: %s\n", argv[i]);
                return 1;
            }
        } else {
            path_image = argv[i];
            if(arg_left > 0) {
                arg_files = ++i;
            }
            break;
        }
    }

    if(!path_image) {
        fprintf(stderr, "%s [OPTIONS] OUTPUT [FILES...]\n", argv[0]);
        return 1;
    }

    DOS_DIRENT dirent_vol;
    if(volume_label) {
        memset(&dirent_vol, 0, sizeof(dirent_vol));
        dirent_vol.mtime = ctime;
        dirent_vol.attributes = FAT_ATTR_LABEL;
        if (fat_name_to_dirent(&dirent_vol, volume_label)){
            fprintf(stderr, "Error: invalid characters in volume label: %s\n", volume_label);
            return 1;
        }
    }

    if(path_bootsector) {
        size_t size_bootsector = 1024;
        bootsector = malloc(size_bootsector);
        memset(bootsector, 0, size_bootsector);
        int fd0 = open(path_bootsector, O_RDONLY);
        if(fd0 == -1) {
            fprintf(stderr, "Error: %s: No such file\n", path_bootsector);
            return 1;
        }
        read(fd0, bootsector, size_bootsector);
        close(fd0);
    }

    int fd = open(path_image, O_CREAT|O_RDWR|O_TRUNC, 0644);
    if(fd == -1) {
        fprintf(stderr, "Error: couldn't create: '%s'\n", path_image);
        return 1;
    }

    uint8_t* image = create_disk_image(&dpb, current_bpb, bootsector, volume_label ? &dirent_vol : NULL);
    if(!image) {
        fprintf(stderr, "Error: creating image\n");
        return 1;
    }
    printf("Creating image: %dKB [CHR %d %d %d] %d b/sec %d b/clus total %d\n",
        (dpb.bpb.total_sectors * dpb.bpb.bytes_per_sector) / 1024,
        dpb.bpb.total_sectors / ( dpb.bpb.n_heads * dpb.bpb.sectors_per_track),
        dpb.bpb.n_heads,
        dpb.bpb.sectors_per_track,
        dpb.bpb.bytes_per_sector, dpb.bytes_per_cluster, dpb.total_clusters
    );
    // printf("DPB: %02x %08x %08x %08x\n", dpb.bpb.media_descriptor, dpb.offset_fat, dpb.offset_root, dpb.offset_cluster);

    if (arg_files) {
        for (int i=arg_files; i<argc; i++) {
            const char* filename = argv[i];
            DOS_DIRENT dirent;
            memset(&dirent, 0, sizeof(dirent));
            dirent.attributes = FAT_ATTR_ARCHIVE;

            if (fat_name_to_dirent(&dirent, basename(filename))) {
                fprintf(stderr, "Error: file name violation in: '%s'\n", filename);
                return 1;
            }
            int fd1 = open(filename, O_RDONLY);
            if(fd1 == -1) {
                fprintf(stderr, "Error: %s: no such file or directory\n", filename);
                return 1;
            }

            if(opt_touchall) {
                dirent.mtime = ctime;
            }else{
                struct stat stat;
                fstat(fd1, &stat);
                dirent.mtime = time_to_dos_file_time(&stat.st_mtime);
            }

            if(fat_write_fd(image, &dpb, &dirent, fd1)) {
                fprintf(stderr, "Error: error copying %s\n", filename);
                return 1;
            }
            close(fd1);

            DOS_DIRENT* dir = (DOS_DIRENT*)(image + dpb.offset_root);
            if(file_exists(dir, dpb.bpb.root_entries_count, &dirent)) {
                fprintf(stderr, "Error: %s: already exists\n", argv[i]);
                return 1;
            }

            for(int i=0; i<dpb.bpb.root_entries_count; i++) {
                if(dir[i].filename[0] == '\0') {
                    dir[i] = dirent;
                    break;
                }
            }

        }
    }

    //  FAT Mirror
    memcpy(image + dpb.offset_fat + dpb.bpb.sectors_per_fat * dpb.bpb.bytes_per_sector, image + dpb.offset_fat, dpb.bpb.sectors_per_fat * dpb.bpb.bytes_per_sector);

    //  Write buffer
    write(fd, image, current_bpb->total_sectors * current_bpb->bytes_per_sector);
    close(fd);
    free(image);
    if(bootsector){ free(bootsector); }

    return 0;
}
