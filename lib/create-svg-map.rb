# From https://campaignwiki.org/traveller/uwp/1215531970
#
# Xuxufla          0101  B7B5000-0     NSG   Ba Fl A
# Rhorho           0103  B99A258-10     CG   Lo Wa
# Lala             0106  D445233-3      SG   Lo Lt              [Ngachu]
# Nowha            0108  E6A0000-0       G   Ba De A            [Ngachu]
# Stitha           0110  B400851-10    NSG   Na Va              [Ngachu]
# Zhuspe           0204  C465333-6       G   Lo                 [Ngachu]
# Zizhisu          0207  E545AB6-6       G   Ga Hi In           [Ziloso]
# Stichu           0209  C653763-4       G   Lt Po              [Ngachu]
# Judretwi         0303  B413467-13      N   Ht Ic Ni
# Ghela            0306  C242545-5     SPG   Lt Ni Po           [Ngachu]
# Thala            0309  C444699-6       G   Ag Ni A            [Ngachu]
# Fletha           0310  C444264-5       G   Lo Lt              [Ngachu]
# Staghi           0404  B352352-13    NTG   Ht Lo Po           [Chofo]
# Thachu           0408  C683463-9       G   Ni                 [Ngachu]
# Fleno            0409  D455474-7      SG   Ni A               [Ngachu]
# Thachu           0410  A8B0672-11    RTC   De Ni A            [Ngachu]
# Ngejo            0501  B210000-0     TCG   Ba
# Twidre           0502  C502468-9      SG   Ic Ni Va
# Spela            0507  C744233-9       G   Ga Lo              [Ngachu]
# Nophi            0510  X560222-2       G   De Lo Lt           [Ngachu]
# Xutwi            0601  C333312-6       S   Lo Po
# Jugigi           0602  D445000-0      PG   Ba
# Ngenuju          0604  E553544-2       G   Lt Ni Po
# Flesti           0607  C72A412-10    CPG   Ni Wa              [Ngachu]
# Ngabogho         0608  E98A121-5       G   Lo Lt Wa           [Ngachu]
# Phivu            0609  B345524-9   NSTCG   Ag Ni              [Ngachu]
# Drena            0702  B5A0496-8       G   De Ni A
# Rhojuju          0703  E58577B-7       G   Ag Ga Ri A
# Juze             0704  C739766-9       S
# Whaphi           0705  C371334-7     SCG   Lo                 [Ngachu]
# Ghozhu           0707  D462761-4       G   Lt Ri              [Ngachu]
# Spezhu           0710  C527486-5       R   Lt Ni              [Ngachu]
# Cezhe            0803  C738785-5      RG   Lt
# Pephi            0805  D74A487-9       S   Ni Wa              [Ngachu]
# Ngavu            0808  D35268A-7      SG   Ni Po A            [Ngachu]
# Ghofla           0809  X635000-0           Ba                 [Ngachu]
# Chuwo            0810  C400510-12     SG   Ht Ni Va A         [Ngachu]
#                        ||||||| |       |
# Ag Agricultural        ||||||| |    Bases     In Industrial
# As Asteroid            ||||||| +- Tech        Lo Low Population
# Ba Barren              ||||||+- Law           Lt Low Technology
# De Desert              |||||+- Government     Na Non-Agricultural
# Fl Fluid Oceans        ||||+- Population      Ni Non-Industrial
# Ga Garden              |||+- Hydro            Po Poor
# Hi High Population     ||+- Atmosphere        Ri Rich
# Ht High Technology     |+- Size               Va Vacuum
# Ic Ice-Capped          +- Starport            Wa Water World
#
# Bases: Naval – Scout – Research – TAS – Consulate – Pirate – Gas Giant

# Perimeter Agency (TAS Facility)
# Psychic Academy (Naval Base)

require 'json'
content = File.read(File.expand_path("../../Thel-Sector.json", __FILE__))
json = JSON.parse(content)

json.fetch("Systems").each do |system|

  # The Traveller format starts indices at 1; The SWNT format starts at 0.
  column = system.fetch('Col').to_i + 1
  row = system.fetch('Row').to_i + 1

  is_hegemon = ! system.fetch("Worlds").map { |w| w.fetch("Tags").map {|t| t.fetch("Name")} }.flatten.grep(/Hegemon/).empty?

  puts "#{system.fetch('Name')}" if is_hegemon

  # Render the initial system
  # puts  sprintf("%s\t%02d%02d", system.fetch("Name"), column, row)
end
