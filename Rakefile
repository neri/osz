#
# Rakefile for OSZ
# Copyright(C)2014-2018 MEG-OS project, ALL RIGHTS RESERVED.
#
require 'rake/clean'
require 'rake/packagetask'

PATH_OUTPUT     = "bin/"
PATH_SRC        = "src/"
PATH_TOOLS      = "tools/"
PATH_TEMP       = "temp/"
PATH_INCLUDE    = "#{ PATH_SRC}include/"

CC		          = ENV['CC'] || "clang"
CFLAGS          = "-Os"
AS		          = ENV['AS'] || "nasm"
AFLAGS	        = "-s -I #{ PATH_INCLUDE } -f bin"

APP_EXT			    = ".com"
DEFAULT_IMG_EXT = ".img"

PATH_FULL_IMG   = "#{PATH_OUTPUT}full#{ DEFAULT_IMG_EXT }"
PATH_MKFDFS     = "#{PATH_TOOLS}mkfdfs"
PATH_BIM2BIN    = "#{PATH_TOOLS}bim2bin"


SRCS = FileList["**/*.c"] << FileList["**/*.cpp"] << FileList["**/*.asm"]
OBJS = SRCS.ext('o')

CLEAN.include(FileList["#{PATH_OUTPUT}/**/*"])
# CLEAN.include(PATH_MKFDFS, PATH_BIM2BIN)

directory PATH_OUTPUT
directory PATH_TEMP

TASKS = [ :tools, :osz ]

TASKS.each do |t|
  task t => [t.to_s + ":build"]
end

desc "Defaults"
task :default => [PATH_OUTPUT, PATH_TEMP, TASKS].flatten


desc "Run with 8086run"
task :run => :default do
  sh "#{ PATH_TOOLS }8086run/8086run -strict #{ PATH_FULL_IMG }"
end


desc "Run with qemu"
task :runqemu => :default do
  sh "qemu-system-x86_64 -cpu max -k ja -m 256 -boot a -fda #{ PATH_FULL_IMG } -rtc base=localtime,clock=host -M pc"
end


####
# tools
namespace :tools do

  targets = [ PATH_MKFDFS, PATH_BIM2BIN ]

  desc "Build Tools"
  task :build => [targets].flatten

  file "#{PATH_MKFDFS}" => "#{PATH_TOOLS}mkfdfs.c" do |t|
    sh "#{ CC } #{ CFLAGS } -o #{t.name} #{t.prerequisites.join(' ')}"
  end

  file "#{PATH_BIM2BIN}" => "#{PATH_TOOLS}bim2bin.c" do |t|
    sh "#{ CC } #{ CFLAGS } -o #{t.name} #{t.prerequisites.join(' ')}"
  end

end

def make_disk(output, ipl, format, label, files)
  file output => [ 'Rakefile', PATH_MKFDFS, ipl, files].flatten do |t|
    #puts "MAKEDISK #{ output } <= #{ ipl } #{ files }"
    sh "#{ PATH_MKFDFS } -bs #{ ipl } -f #{ format } -l #{ label } #{ t.name } '#{ files.join("' '") }'"
  end
end


####
# OSZ
namespace :osz do

  PATH_SRC_BOOT = "#{ PATH_SRC }boot/"
  PATH_SRC_SYS  = "#{ PATH_SRC }sys/"
  PATH_SRC_APP  = "#{ PATH_SRC }app/"
  PATH_OSZ_INC  = FileList["#{ PATH_INCLUDE }**.inc"]

  PATH_BIN_SYS  = "#{ PATH_OUTPUT }sys/"
  PATH_BIN_APP  = "#{ PATH_OUTPUT }app/"

  ALL_OBJS = []

  directory PATH_BIN_SYS
  directory PATH_BIN_APP

  PATH_OS_SYS 	= "#{ PATH_BIN_SYS }kernel.sys"

  def binread_unpack(filename)
    bin = nil
    File.open(filename) do |file|
      bin = file.read(File.size(filename)).unpack('C*')
    end
    bin
  end

  def emit_le16(val)
    [ (val & 0xFF), (val >> 8) & 0xFF ]
  end

  def to_FCB_name(name, namelentgh=8, extlength=3)
    sprintf('%-*.*s%-*.*s', namelentgh, namelentgh, File.basename(name, '.*'), extlength, extlength, File.extname(name)[1, extlength])
  end

  def emit_padding(size, padding=16)
    if ((size % padding) > 0) then
      Array.new(padding-(size % padding), 0)
    else
      []
    end
  end

  # make system mods (bioses, etc...)
  def make_mod(name)
    t = name
    bin = "#{ PATH_BIN_SYS }#{ t }.bin"
    src = "#{ PATH_SRC_SYS }#{ t }.asm"
    file bin => [src, PATH_OSZ_INC, PATH_BIN_SYS].flatten do |t|
      sh "#{ AS } #{ AFLAGS } -o #{t.name} #{ src }"
    end
    ALL_OBJS.push bin
    bin
  end

  # make normal apps
  def make_app(name)
    make_app_sp do
      bin = "#{ PATH_TEMP }#{ name }.bin"
      src = "#{ PATH_SRC_APP }#{ name }.asm"
      file bin => [src, PATH_OSZ_INC, PATH_BIN_APP].flatten do |t|
        sh "#{ AS } #{ AFLAGS } -o #{ t.name } #{ src }"
      end
      bin
    end
  end

  # make apps from bin
  def make_app_sp
    bin = yield
    name = File.basename(bin, '.*')
    temp = "#{ PATH_TEMP }#{ name }.tek"
    out = "#{ PATH_BIN_APP }#{ name }#{ APP_EXT }"
    # file temp => [bin].flatten do |t|
    #   sh "#{ PATH_BIM2BIN } -osacmp -tek1 BS:0 in:#{ bin } out:#{ temp }"
    # end
    file out => [bin] do |t|
      sh "cp #{ bin } #{ out }"
    end
    ALL_OBJS.push out
    out
  end

  def make_kernel(output, locore, bioses, bdos, init_files)
    file output => [ 'Rakefile', locore, bioses, bdos, init_files, PATH_BIN_SYS].flatten do |t|

      bin_locore = binread_unpack(locore)
      puts "Make Kernel with: #{ locore } $#{ bin_locore.length.to_s(16) } (#{ bin_locore.length })"

      bioses << bdos

      # bioses
      tbl = []
      blob = []
      bioses.each do |file|
        bin = binread_unpack(file)
        puts " BIOS: #{ file } ($#{ bin.length.to_s(16) } #{ bin.length })"
        tbl += emit_le16(bin.length)
        blob += bin
      end
      tbl += emit_le16(0)
      offset1 = bin_locore.length + tbl.length
      offset2 = offset1 + blob.length

      # PADDING
      blob += emit_padding(offset2)
      offset2 = offset1 + blob.length

      # initrd
      rdblob = []
      dir = []
      n_initfiles = 0
      init_files.each_with_index do |file, index|
        modname = File.basename(file)
        bin = binread_unpack(file)
        puts " INITRD: #{modname} (#{ file } $#{ '%04x' % rdblob.length } $#{ '%04x' % bin.length } #{ bin.length })"
        dir += to_FCB_name(modname).upcase.unpack('C*') + [0] + emit_le16(rdblob.length) + emit_le16(bin.length)
        rdblob += bin
        n_initfiles += 1
      end
      rdblob += emit_padding(rdblob.length)
      dir += emit_le16(rdblob.length) + emit_le16(n_initfiles)
      dir += emit_padding(dir.length)
      initrd = rdblob + dir
      size_initrd = initrd.length

      obj = bin_locore + tbl + blob + initrd
      size = obj.length
      raise "#{ output }: Out of Segment!" if size >= 0x10000

      obj[4, 2] = emit_le16(size)
      obj[6, 2] = emit_le16(offset1)
      obj[8, 2] = emit_le16(offset2)
      obj[10, 2] = emit_le16(size_initrd)

      puts "OUTPUT: $#{ obj.length.to_s(16) } (#{ obj.length })"
      #IO.write(t.name, obj.pack('C*'))
      File.open(t.name, 'w') do |file|
	      file.write(obj.pack('C*'))
      end
    end
  end


  # normal apps
  APP_DEFAULTS = %w( hello chars echo2 bf pipo mon cpuid edit midi ).collect {|t| make_app(t) }
  APP_NO_DEFAULTS = %w( cpuid ).collect {|t| make_app(t) }

  # misc
  MISC = [
    FileList["src/misc/*"]
  ].flatten


  # tfdisk
  PATH_TFDISK_BIN = make_app_sp do
    main_src = "#{ PATH_SRC }tfdisk/tfdisk.asm"
    tfdisk_bin = "#{ PATH_TEMP }tfdisk.bin"

    tfmbr_bin = "#{ PATH_TEMP }tfmbr.bin"
    exipl_bin = "#{ PATH_TEMP }exboot.bin"
    ipl16_bin = "#{ PATH_TEMP }hdbt16.bin"
    ipl32_bin = "#{ PATH_TEMP }hdbt32.bin"

    [tfmbr_bin, exipl_bin, ipl16_bin, ipl32_bin].each do |bin|
      src = PATH_SRC + "tfdisk/" + File.basename(bin, ".bin") + ".asm"
      file bin => src do |t|
        sh "#{ AS } #{ AFLAGS } -o #{t.name} #{t.prerequisites.join(' ')}"
      end
    end

    file tfdisk_bin => [main_src, tfmbr_bin, exipl_bin, ipl16_bin, ipl32_bin, PATH_OSZ_INC].flatten do |t|
      sh ["#{ AS } #{ AFLAGS } -i #{ PATH_SRC} -o #{t.name}",
        "-DPATH_MBR=\\\"#{File.expand_path(tfmbr_bin)}\\\"",
        "-DPATH_EXIPL=\\\"#{File.expand_path(exipl_bin)}\\\"",
        "-DPATH_IPL16=\\\"#{File.expand_path(ipl16_bin)}\\\"",
        "-DPATH_IPL32=\\\"#{File.expand_path(ipl32_bin)}\\\"",
        main_src].join(' ')
    end

    tfdisk_bin
  end
  ALL_OBJS << PATH_TFDISK_BIN
  APP_NO_DEFAULTS << PATH_TFDISK_BIN


  # kernel
  make_kernel( PATH_OS_SYS, make_mod('osz2bt'),
    %w( oszbio oszn98 oszfmt oszacpi ).collect {|t| make_mod(t) },
    make_mod('oszdos'),
    [
      make_mod('init'),
      %w( zcom pipo hello chars echo2 cpuid midi ).sort.collect {|t| make_app(t) },
      PATH_TFDISK_BIN
    ].flatten
  )

  # boots
  PATH_BIN_BOOT = "#{ PATH_OUTPUT }boot/"
  directory PATH_BIN_BOOT
  IPLS = {}
  %w(fdipl fdboot).each do |ipl|
    srcipl = "#{ PATH_SRC_BOOT }#{ ipl }.asm"
    output = "#{ PATH_BIN_BOOT }#{ ipl }.bin"
    file output => [PATH_BIN_BOOT, srcipl] do |t|
      sh "#{ AS } #{ AFLAGS } -o #{t.name} #{ srcipl }"
    end

    IPLS[ipl.to_sym] = output
  end


  # images
  APP_FULL = [APP_DEFAULTS, APP_NO_DEFAULTS].flatten
  ALL_IMAGES = [
    { :name => :tiny, :format => 160, :files => [APP_DEFAULTS, MISC] },
    { :name => :mini, :format => 720, :files => [APP_DEFAULTS, MISC] },
    { :name => 'mini640.hdm', :format => 640, :files => [APP_DEFAULTS, MISC] },
    { :name => 'full98.hdm', :format => 1232, :files => [APP_FULL, MISC] },
    { :name => :full, :format => 1440, :files => [APP_FULL, MISC] }
  ].map do |imgdef|
    imgname = imgdef[:name].to_s
    imgname << DEFAULT_IMG_EXT if File.extname(imgname) == ''
    output = "#{ PATH_OUTPUT }#{ imgname }"
    files = [imgdef[:files]].flatten.uniq.sort {|a, b| File.basename(a).upcase <=> File.basename(b).upcase }
    files.unshift PATH_OS_SYS
    make_disk output, IPLS[:fdipl], imgdef[:format]||'2hd', 'osz', files
    output
  end


  desc "Build OSZ"
  task :build => [ALL_OBJS, ALL_IMAGES].flatten

end
